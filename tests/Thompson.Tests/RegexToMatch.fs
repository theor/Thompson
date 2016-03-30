module RegexToMatch

open Thompson
open Thompson.Automatas

open NUnit.Framework
open FsUnitTyped
open FsCheck
open FsCheck.NUnit

type Tcs() =
    static member tcs : obj array array = [|
        [| "ab*a"; "a"; false |]
        [| "ab*a"; "ab"; false |]
        [| "ab*a"; "abb"; false |]
        [| "ab*a"; "aa"; true |]
        [| "ab*a"; "aba"; true |]
        [| "ab*a"; "abba"; true |]
        [| "ab*a"; "abbba"; true |]

        [| "a+"; ""; false |]
        [| "a+"; "b"; false |]
        [| "a+"; "a"; true |]
        [| "a+"; "aa"; true |]
        [| "a+"; "aaa"; true |]
    |]

[<Test>]
[<TestCaseSource(typedefof<Tcs>, "tcs")>]
let ``is match`` (pattern:string) (input:string) (isMatch:bool) =
    let r = Regex(pattern)
    r.isMatch input |> shouldEqual isMatch

[<Test>]
let ``. < a`` () =
    let r = Regex(".")
    r.isMatch "a" |> shouldEqual true
