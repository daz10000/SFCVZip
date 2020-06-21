open System
open System.Globalization
open System.Text
open System.IO
open FSharp.Data
open Argu
open SFZip.polyCoords
open MathNet.Numerics

type SFZipFormats = CsvProvider<"sfzip_20200511.csv">
type RateCsv = CsvProvider<"rate_example.csv">

type CLIArguments =
    | Convert of string
    | Fit
    | Draw
    | Test
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Convert _ -> "convert raw zip data to coordinate list"
            | Draw -> "draw svg"
            | Fit -> "draw svg"
            | Test -> "debugging"



type DataPoint = { Date : DateTime ; Zip: int ; Cases: int }

type RateData = { Id : int ; Rate : float ; Cases : int}

type RGB = {
    R:int
    G:int
    B:int
}

let white = { R = 255 ; G = 255; B = 255 }
let legend = [| 0.0001 ; 1.0025; 1.005;1.0075;1.01;1.015;1.02;1.025;1.03;1.035;1.04;1.045;1.05;1.06;9999.0|]
let colorsRaw = [| "#ffffff" ; "#dcdcdc"; "#d0d6cd"; "#bdc9be"; "#aabdaf"; "#97b0a0"; "#84a491"; "#719782"; "#5e8b73"; "#4b7e64"; "#387255"; "#256546"; "#125937"; "#004d28" ; "#002d08"|]
let colors = colorsRaw |> Array.map ( fun text ->
                        {
                            R= Int32.Parse(text.[1..2],NumberStyles.HexNumber)
                            G= Int32.Parse(text.[3..4],NumberStyles.HexNumber)
                            B= Int32.Parse(text.[5..6],NumberStyles.HexNumber)
                        }
                 )
let lookupColor (v:float) =
    Array.zip colors legend |> Array.find (fun (rgb,value) -> value > v) |> fst

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "SFZip.exe")
    let args = parser.Parse argv

    if args.Contains Fit then
        printf "Fitting data"
        let data = [|     for file in System.IO.Directory.EnumerateFiles(".","sfzip_20*.csv") do
                                printfn "  Processing %s" file
                                for row in SFZipFormats.Parse(File.ReadAllText(file)).Rows do
                                        yield { Zip = row.``ZIP Code`` ; Date = row.``Data as of`` ; Cases = (row.``Count of Confirmed Cases`` |> Option.ofNullable |> Option.defaultValue 0)}
                    |] |> Array.distinctBy (fun x -> x.Zip,x.Date)

        let lookupByZipAndDate = [| for x in data -> (x.Zip,x.Date),x.Cases |] |> Map.ofArray

        let allDates = [| for x in data -> x.Date |] |> Array.distinct |> Array.sort
        let allZips = [| for x in data -> x.Zip |] |> Array.distinct |> Array.sort
        let badDates =
            [|  for i in 1..allDates.Length-1 do
                    let thisDate = allDates.[i].Date
                    let lastDate = allDates.[i-1].Date
                    if allZips |> Array.forall (fun z -> lookupByZipAndDate.[(z,thisDate)] = lookupByZipAndDate.[z,lastDate]) then
                      yield thisDate // bad data point
            |] |> Set.ofArray

        for d in allDates do
            if badDates.Contains d then
                printfn "    omitting dummy date %A" d
            else
                printfn "    considering date %A" d

        use outF2 = new StreamWriter("raw.csv")
        outF2.Write("date")
        outF2.WriteLine(","+String.Join(",",allZips))
        for date in allDates do
            if not <| badDates.Contains date then
                outF2.Write(date)
                outF2.WriteLine(","+String.Join(",",[| for z in allZips -> lookupByZipAndDate.[(z,date)] |> string |]))
        outF2.Close()

        printfn "Selecting data"
        let zipGroup = 
            data 
            |> Array.groupBy (fun x -> x.Zip) 
            |> Array.choose (fun (zip,points) -> 
                                    let usable = points |> Seq.filter(fun p->(not<| badDates.Contains p.Date) && p.Cases>0) 
                                    if Seq.length usable >= 2 then Some (zip,points) else None
                            )

        printfn "Fitting rates"
        let rates =
            zipGroup
            |> Array.choose (fun (zip,points) ->
                            let sortedPoints = points |> Seq.filter (fun x -> x.Cases>0 && not <| badDates.Contains x.Date) |> Seq.sortBy (fun x -> x.Date) |> Array.ofSeq
                            let lastDate = sortedPoints.[sortedPoints.Length-1].Date
                            let earliest = sortedPoints.[sortedPoints.Length-2].Date |> min (lastDate.AddDays(-7.0))
                            let usedPoints = sortedPoints |> Array.filter(fun x -> x.Date>=earliest)
                            // printfn "zip %d: last=%s penultimate=%s earliest=%s %d points available"  
                            //     zip 
                            //     (lastDate.ToShortDateString()) 
                            //     (points.[points.Length-2].Date.ToShortDateString())
                            //     (earliest.ToShortDateString()) 
                            //     usedPoints.Length
                            let x = [| for p in usedPoints -> (p.Date-earliest).TotalDays |]
                            let y = [| for p in usedPoints -> log10 (float p.Cases) |]
                            if x.Length >= 2 then
                                let c,m = Fit.Line(x,y)
                                Some
                                    {   Rate = 10.0**m
                                        Id = zip
                                        Cases = points.[points.Length-1].Cases
                                    }
                            else
                                // not enough points to fit
                                None
                )
        use outF = new StreamWriter("rates.csv")
        outF.WriteLine("zip,rate,count")
        for r in rates do
            outF.WriteLine(sprintf "%d,%f,%d" r.Id r.Rate r.Cases)

    if args.Contains Test then
        tests()
        exit 0

    if args.Contains Convert then
        let inFile = args.GetResult Convert

        let data = SFZipFormats.Parse(File.ReadAllText(inFile))
        use outF = new StreamWriter("polygons.txt")
        for row in data.Rows do
            outF.WriteLine(sprintf "%d\t%s" row.``ZIP Code`` row.Multipolygon) // (String.Join(",",points)))

    let floatCheck (s:string) =
        match Double.TryParse s with
        | false,_ ->
            failwithf "ERROR in '%s'" s
        | true,v -> v

    if args.Contains Draw then
        printf "Drawing data"
        let zips =
            [| for row in File.ReadAllLines("polygons.txt") ->
                let cols = row.Split([|'\t'|])
                let coords = parsePOLYGON (cols.[1])
                {   Zipcode = cols.[0] |> int
                    Outline = coords
                }

            |]

        use outF = new StreamWriter("sfzip.html")

        let rec walk (c:CoordSet) =
            seq {    match c with
                     | Single s -> yield! s
                     | Set (a,b) ->
                        yield! walk a
                        yield! walk b
                }

        let allCoords = seq {
                            for z in zips do
                                yield! walk z.Outline
                        } |> Array.ofSeq

        let minLat = allCoords |> Array.minBy (fun x -> x.Lat)
        let maxLat = allCoords |> Array.maxBy (fun x -> x.Lat)
        let minLon = allCoords |> Array.minBy (fun x -> x.Lon)
        let maxLon = allCoords |> Array.maxBy (fun x -> x.Lon)


        let margin = 30.0
        let width = 500.0
        let height = 400.0
        let scaleX = (width-(margin*2.0)) / (maxLon.Lon-minLon.Lon)
        let scaleY = (height-(margin*2.0)) / (maxLat.Lat-minLat.Lat)
        let toX x = (x-minLon.Lon)*scaleX+margin
        let toY y = height-margin-(y-minLat.Lat)*scaleY

        outF.WriteLine("""<html>
                       <head>
  <link rel="stylesheet" type="text/css" href="tooltipster.bundle.min.css"/>
  <link rel="stylesheet" type="text/css" href="tooltipster-sideTip-punk.min.css"/>
  <script type="text/javascript" src="jquery-1.10.0.min.js"></script>
  <script type="text/javascript" src="tooltipster.bundle.min.js"></script>
  <script>
    $(document).ready(function() {
        $('.tooltip').tooltipster({
        theme: 'tooltipster-punk',
        contentAsHTML: true
        });
    });
</script>
</head> """)

        outF.WriteLine(sprintf "<svg viewBox=\"0 0 %f %f\">" width height)

        let rec draw id (label:string) (color:RGB) (c:CoordSet) =
            match c with
            | Single s ->
                let start = List.head s
                let startX = start.Lon |> toX
                let startY = start.Lat |> toY
                outF.WriteLine(sprintf """<path id="%d" d="M %f %f """ id startX startY)
                s.Tail |> List.iter(fun c ->
                    outF.WriteLine(sprintf """L %f %f""" (c.Lon |> toX)  (c.Lat |> toY))
                    )
                outF.WriteLine(sprintf """" fill="rgb(%d,%d,%d)" stroke="rgb(0,100,100)" stroke-width="1"/>""" color.R color.G color.B)
                (id+1)
            | Set(a,b) ->
                let id' = draw id label color a
                draw id' label color b

        let rates = // rateData.Split([|'\n'|]) |> Array.map (fun row -> row.Trim().Split([|' ';'\t'|],StringSplitOptions.RemoveEmptyEntries)) |> Array.map(fun cols -> {Id = int (cols.[0])  ; Rate = float (cols.[1])})
            [| for row in RateCsv.Parse(File.ReadAllText("rates.csv")).Rows -> {Id = row.Zip ; Rate = row.Rate |> float ; Cases = row.Cases} |]
        let rateLookups = [for r in rates -> r.Id,r.Rate] |> Map.ofList
        let caseLookups = [for r in rates -> r.Id,r.Cases] |> Map.ofList


        for z in zips do
            let rgb = rateLookups.TryFind z.Zipcode |> Option.map lookupColor |> Option.defaultValue white
            let rate = match rateLookups.TryFind z.Zipcode with | Some x -> (x-1.0)*100.0 | None -> 0.0
            let cases = caseLookups.TryFind z.Zipcode |> Option.defaultValue 0
            // let rate = rateLookups.TryFind z.Zipcode |> Option.defaultValue 0.0
            let label = sprintf "zip:%d rate=%3.2f%% cases=%d" z.Zipcode rate cases
            outF.WriteLine(sprintf """<g class="tooltip" title="%s" >""" label)
            draw 1 label rgb z.Outline |> ignore
            outF.WriteLine(sprintf """</g>""")

        outF.WriteLine("<g>")
        Array.zip colorsRaw legend
        |> Array.iteri (fun i (color,value) ->
                            let x = margin+(float i)*20.0
                            let y = 20.0
                            outF.WriteLine(sprintf """
                             <g>
                                <text x="%f" y="%f" font-family="Verdana" font-size="3" fill="blue">&lt;%f</text>
                                <rect x="%f" y="%f" width="10" height="10" fill="%s" stroke="black"></rect>
                              </g>
                            """ x (y+15.0) value x y color)
            )
        outF.WriteLine("</g>")

        outF.WriteLine("</svg>")
        outF.WriteLine("</html>")

    0
