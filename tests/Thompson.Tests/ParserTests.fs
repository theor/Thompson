module ParserTests

open Thompson
open Thompson.Regex
open NUnit.Framework
open FsUnitTyped
open FParsec

let c = Val << Char

let test str expRegex =
    match Parser.parse str with
    | Success (op,_,pos) -> 
        printfn "%A" op
        op |> shouldEqual expRegex
    | Failure(s,error,_) -> Assert.Fail(error.ToString())
[<Test>]
let ``parse ab`` () =
    test "ab" (Concat(c 'a', c 'b'))
[<Test>]
let ``parse a*|b|c`` () =
    test "a*|b|c" (Union(Kleene(c 'a'), Union(c 'b', c 'c')))

[<Test>]
let ``parse ab|abc`` () =
    test "ab|abc" (Union(
                    Concat(c 'a', c 'b'),
                    Concat(Concat(c 'a', c 'b'), c 'c')))