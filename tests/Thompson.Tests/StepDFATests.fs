namespace Thompson.Tests

open NUnit.Framework
open FsUnitTyped
open FsCheck
open FsCheck.NUnit

open Thompson
open Thompson.Regex

module StepDFATests =

    [<Test>]
    let ``a`` () =
        let s0 = Set [0]
        let s1 = Set [1]
        let dfa = Automata.emptyDFA |> Automata.addTransition s0 (Char 'a') s1
                                    |> Automata.addTransition s1 (Char 'a') s1
                                    |> Automata.setInitState s0
                                    |> Automata.addEndState s1
        Automata.stepDFA dfa (Char 'a') s0 |> shouldEqual (Some s1)
        Automata.stepDFA dfa (Char 'a') s1 |> shouldEqual (Some s1)
        Automata.stepDFA dfa (Char 'b') s0 |> shouldEqual None
        Automata.stepDFA dfa (Char 'b') s1 |> shouldEqual None
        Automata.stepDFA dfa Epsilon s0 |> shouldEqual None
        Automata.stepDFA dfa Epsilon s1 |> shouldEqual None

        Automata.isMatchDFA dfa "a" |> shouldEqual true
        Automata.isMatchDFA dfa "aa" |> shouldEqual true
        Automata.isMatchDFA dfa "aaaa" |> shouldEqual true
        
        Automata.isMatchDFA dfa "" |> shouldEqual false
        Automata.isMatchDFA dfa "b" |> shouldEqual false
        Automata.isMatchDFA dfa "ab" |> shouldEqual false

    [<Property>]
    let ``add state to dfa`` (xs:int list) =
        List.rev(List.rev xs) = xs