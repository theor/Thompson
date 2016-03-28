module EpsilonRemovalTests

open Thompson
open Thompson.Regex
open NUnit.Framework
open FsUnitTyped

let test nfa expNfa =
    nfa |> Automatas.epsilonRemoval |> shouldEqual (Some(expNfa))
[<Test>]
let ``a*`` () =
    let nfa = Automata.emptyNFA
                |> Automata.setInitState 0
                |> Automata.addTransition 0 (Char 'a') 0
                |> Automata.addTransition 0 Epsilon 1
                |> Automata.addEndState 1
    let s01 = (Set [0;1])
    let expNfa = Automata.emptyDFA
                |> Automata.setInitState s01
                |> Automata.addTransition s01 (Char 'a') s01
                |> Automata.addEndState s01
    test nfa expNfa
[<Test>]
let ``ab*`` () =
    let nfa = Automata.emptyNFA
                |> Automata.setInitState 1
                |> Automata.addTransition 1 Epsilon 2
                |> Automata.addTransition 2 (Char 'a') 3
                |> Automata.addTransition 3 Epsilon 4
                |> Automata.addTransition 4 (Char 'b') 5
                |> Automata.addTransition 5 Epsilon 6
                |> Automata.addTransition 5 Epsilon 2
                |> Automata.addTransition 1 Epsilon 6
                |> Automata.addEndState 6
    let s126 = (Set [1;2;6])
    let s34 = (Set [3;4])
    let s256 = (Set [2;5;6])
    let expNfa = Automata.emptyDFA
                |> Automata.setInitState s126
                |> Automata.addTransition s126 (Char 'a') s34
                |> Automata.addTransition s34 (Char 'b') s256
                |> Automata.addTransition s256 (Char 'a') s34
                |> Automata.addEndState s256
    test nfa expNfa