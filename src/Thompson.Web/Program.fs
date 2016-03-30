// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Suave
open Suave.Filters
open Suave.Files
open Suave.Operators
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Thompson
open Thompson.Regex
open Thompson.Automata

let nfaToJson (nfa:FSM<_> option) =
    let mapend ends =
        ends |> Seq.map (fun x ->
            let v = match box x with | :? Set<int> as s -> JArray(s) :> JToken | _ as o -> JValue(o) :> JToken
            v)
             |> JArray
    let mapOpand (o:Opand) =
        match o with
        | Char c -> Some c
        | Any -> Some '.'
        | _ -> None

    let mapTransition ((f,c),l:_ list) =
        l |> List.map (fun t ->
            let o = new JObject(
                        JProperty("from", f :> obj),
                        JProperty("to", t :> obj)
            )
            match mapOpand c with
            | Some opand -> o.Add(JProperty("label", opand.ToString()))
            | None -> ()
            o
        )
    match nfa with
    | Some nfa ->
        JObject(
            JProperty("start", nfa.start),
            JProperty("ends", new JArray(mapend nfa.ends)),
            JProperty("transitions",
                JArray(nfa.transitions |> Map.toList |> List.collect mapTransition))
        )
    | None -> null

let JSON v =
  let jsonSerializerSettings = new JsonSerializerSettings()
  jsonSerializerSettings.ContractResolver <- new Serialization.CamelCasePropertyNamesContractResolver()

  JsonConvert.SerializeObject(v, jsonSerializerSettings)
  |> Successful.OK
  >=> Writers.setMimeType "application/json; charset=utf-8"

let webPart nfa =
    choose [
        path "/data.json" >=> GET >=> JSON(nfaToJson nfa)
        path "/about" >=> (Successful.OK "Abount")
        path "/regex" >=> POST >=> request (fun req ->
            let str = req.form |> List.head |> fst
            let r = Parser.parse str
            match r with
            | FParsec.CharParsers.ParserResult.Success(a,b,c) ->
                let nfa = Automatas.toNFA a
                let dfa = nfa |> Option.bind Automatas.epsilonRemoval
                JObject(JProperty("nfa", nfaToJson nfa),
                        JProperty("dfa", nfaToJson dfa))
                |> JSON
            | _ -> Successful.OK "Abount"
        )
        path "/" >=> Redirection.redirect "index.html"
        browseHome
//        path "/store" >=> (OK "Store")
//        path "/store/browse" >=> browse
//        pathScan "/store/details/%d" (fun id -> OK (sprintf "Details %d" id))
    ]

[<EntryPoint>]
let main argv =
//    let nfa = Automata.emptyNFA
//              |> NFA.addTransition 0 Epsilon 1
//              |> NFA.addTransition 1 (Char 'a') 2
//              |> NFA.addTransition 2 Epsilon 5
//              |> NFA.addTransition 0 Epsilon 3
//              |> NFA.addTransition 3 (Char 'b') 4
//              |> NFA.addTransition 4 Epsilon 5
//              |> NFA.addEndState 5
//    let r = Concat(
//                Val(Char 'a'),
//                Concat(Val(Char 'b'), Val(Char 'c'))
//            )
//    let r = Union(Val(Char 'a'),Val(Char 'b'))
//    let r = Union(Concat(Val(Char 'a'),Val(Char 'b')), Kleene(Val(Char 'c')))
//    let nfa = Automatas.toNFA r |> Option.get
    let r = Parser.parse "a*|b|c"
    match r with
    | FParsec.CharParsers.ParserResult.Success(a,b,c) ->
        printfn "%A" a
        let nfa = Automatas.toNFA a
        startWebServer { defaultConfig with homeFolder = Some (System.IO.Path.GetFullPath "..\\..\\") } (webPart nfa)
    | _ -> ()
    printfn "%A" argv
    0 // return an integer exit code
