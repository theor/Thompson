module RegexToMatch

open Thompson
open Thompson.Automatas

open NUnit.Framework
open FsUnitTyped
open FsCheck
open FsCheck.NUnit

[<Test>]
let ``ab*a < aa`` () =
    let r = Regex("ab*a")
    r.isMatch "aa" |> shouldEqual true
    
[<Test>]
let ``ab*a < aba`` () =
    let r = Regex("ab*a")
    r.isMatch "aba" |> shouldEqual true
    
[<Test>]
let ``ab*a < abba`` () =
    let r = Regex("ab*a")
    r.isMatch "abba" |> shouldEqual true
    
[<Test>]
let ``ab*a < abbba`` () =
    let r = Regex("ab*a")
    r.isMatch "abbba" |> shouldEqual true

