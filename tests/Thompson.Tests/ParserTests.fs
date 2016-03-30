module ParserTests

open Thompson
open Thompson.Regex
open NUnit
open NUnit.Framework
open FsUnitTyped
open FParsec

let c = Val << Char

type Tcs() =
    static member tcs : obj array array = [|
            [| "ab"; (Concat(c 'a', c 'b')) |]
            [| "a*|b|c"; (Union(Kleene(c 'a'), Union(c 'b', c 'c'))) |]
            [| "ab|abc"; (Union(
                            Concat(c 'a', c 'b'),
                            Concat(Concat(c 'a', c 'b'), c 'c'))) |]
            [| "a\\+"; (Concat(c 'a', c '+')) |]
            [| "a\\*"; (Concat(c 'a', c '*')) |]
            [| "a(b|c)"; (Concat(c 'a', Union(c 'b', c 'c'))) |]

        |]
let test str expRegex =
    match Parser.parse str with
    | Success (op,_,pos) ->
        printfn "%A" op
        printfn "%s" (Op.format op)
        op |> shouldEqual expRegex
    | Failure(s,error,_) -> Assert.Fail(error.ToString())


[<TestCaseSource(typedefof<Tcs>, "tcs")>]
let testParseAst(str:string, ast:Op) =
    test str ast
