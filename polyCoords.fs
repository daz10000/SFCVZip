module SFZip.polyCoords

open System.Text
type Coord = { Lat:float ; Lon : float}

type CoordSet =
    | Single of Coord list
    | Set of CoordSet*CoordSet

type Zip = { Zipcode : int ; Outline : CoordSet}

/// Whitespace removal
let rec (|Ws|) = function
    | ' '::Ws(rem) -> rem
    | x -> x

type Part =
    | ICONST of int
    | FCONST of double


let (|Atom|_|) = function
    | c::tl1 when '0'<=c && c<='9' ->
        let sb = StringBuilder().Append(c)
        let rec aux count =
                    function
                        | c::t when '0'<=c && c<='9' ->
                                    sb.Append(c) |> ignore
                                    aux count t
                        | '.'::t ->
                                if count > 0 then failwithf "Illegal floating point number %s." (sb.ToString())
                                else
                                    sb.Append('.') |> ignore
                                    aux 1 t
                        | t ->
                            let s = sb.ToString()
                            Some(match count with
                                        | 0 ->ICONST(int(s))
                                        | 1 -> FCONST(double s)
                                        | _ -> failwithf "impossible"
                                ,t
                            )
        aux 0 tl1
    | _ -> None
let (|SignedAtom|_|) = function
    | '-'::Atom(x,rem) ->
        match x with
        | FCONST f -> Some (FCONST(-f),rem)
        | ICONST i -> Some (ICONST(-i),rem)
    | Atom(x,rem) -> Some(x,rem)
    | _ -> None

let (|CoordinatePair|_|) = function
    | SignedAtom(FCONST(lon),Ws(SignedAtom(FCONST(lat),rem))) -> Some ({Lat = lat ; Lon = lon},rem)
    | _ -> None

let coordVerbose = false
let rec (|CoordList|_|) chars =
    if coordVerbose then printfn "Enter CoordList: %A" chars
    match chars with
    | CoordinatePair(p,Ws(rem1)) ->
        match rem1 with
        | ','::Ws(CoordList(l,rem)) ->
            if coordVerbose then printfn "CCC rem=%A l=%A p=%A" rem l p
            Some(p::l,rem)
        | rem ->
            if coordVerbose then printfn "DDD rem=%A" rem
            Some([p],rem)
    | _ -> None


let rec (|CoordSetOrSingle|_|) chars =
    if coordVerbose then printfn "StartCoordSetOrSingle: %A" chars
    match chars with
        | '('::CoordSetOrSingle(set1,Ws(')'::rem)) ->
            if coordVerbose then printfn "AAA rem=%A set1=%A" rem set1
            match rem with
            | ','::Ws(CoordSetOrSingle(set2,rem2)) ->
                Some(Set(set1,set2),rem2)
            | rem3 -> Some(set1,rem3)
        | CoordList(l1,rem) ->
            if coordVerbose then printfn "BBB rem=%A" rem
            Some (Single l1,rem)
        | _ -> None

let parsePOLYGON (s:string) =
    let chars = s.ToCharArray() |> List.ofArray

    match chars with
    | 'M'::'U'::'L'::'T'::'I'::'P'::'O'::'L'::'Y'::'G'::'O'::'N'::Ws(CoordSetOrSingle (c,Ws([]))) -> c
    | _ ->
        failwithf "Failed to parse '%s'" s

let ch (s:string) = s.ToCharArray() |> List.ofArray
let tests() =
    match ch "37.771330999098105" with
    | Atom(x,rem) ->
        printfn "Matched atom %A rem=%A" x rem
    | _ ->
        printfn "failed to match atom"

    match ch "-37.771330999098105" with
    | SignedAtom(x,rem) ->
        printfn "Matched signedatom %A rem=%A" x rem
    | _ ->
        printfn "failed to match atom"

    match ch "-122.51314099995933 37.771330999098105" with
    | CoordinatePair(x,[]) -> printfn "matched coord list pair %A" x
    | _ ->
        printfn "Failed to match"
        exit 1

    match ch "-122.51314099995933 37.771330999098105" with
    | CoordinatePair(x,[]) -> printfn "matched coord list pair %A" x
    | _ ->
        printfn "Failed to match"
        exit 1

    match ch "-122.51314099995933 37.771330999098105, 100.0 200.0" with
    | CoordList(x,rem) -> printfn "%A rem=%A" x rem
    | _ ->
        printfn "Failed to match"
        exit 1

    match ch "-122.51314099995933 37.771330999098105, -100.0 52.5, -200.0 10.4" with
    | CoordList(x,rem) -> printfn "%A rem=%A" x rem
    | _ ->
        printfn "Failed to match"
        exit 1

    match ch "(-122.51314099995933 37.771330999098105, -100.0 52.5, -200.0 10.4)" with
    | CoordSetOrSingle(x,rem) -> printfn "matched CoordSetOrSingle1 %A rem=%A" x rem
    | _ ->
        printfn "Failed to match CoordsSetOrSingle1"
        exit 1

    match ch "(-122.51314099995933 37.771330999098105, -100.0 52.5, -200.0 10.4)" with
    | CoordSetOrSingle(x,rem) -> printfn "matched CoordSetOrSingle2 %A rem=%A" x rem
    | _ ->
        printfn "Failed to match CoordSetOrSingle2"
        exit 1

    match ch "((-122.5131 37.77 ,-100.0 52.5, -200.0 10.4),(1.0 1.0, 1.0 1.0))" with
    | CoordSetOrSingle(x,rem) -> printfn "matched coords3 %A rem=%A" x rem
    | _ ->
        printfn "Failed to match 3"
        exit 1

