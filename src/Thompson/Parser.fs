module Parser

open FParsec
open Thompson.Regex

let parse s =
    let cha = anyOf ['a';'b';'c'] |>> (Char>>Val)
    let elementaryRE = cha
    let star = elementaryRE .>>? pchar '*' |>> Kleene
//        let plus = elementaryRE .>>? pchar '+' |>> Kleene
    let basicRE = star (*<|> plus*) <|> elementaryRE
    let simpleRE = (* concatenation <|>  *) basicRE
//            many1 letter |>> (fun x -> List.tail x |> List.fold (fun s c -> Concat(s, c |> Char |> Val)) (List.head x |> Char |> Val))
    let union, unionImpl = createParserForwardedToRef()
    // = re <|> simpleRE
    let re = union <|> simpleRE
    do unionImpl := simpleRE .>>? pchar '|' .>>.? re |>> Union
//        let union = pchar '|'
    FParsec.CharParsers.run re s

