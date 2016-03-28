module ClosureTests

open Thompson
open Thompson.Regex
open NUnit.Framework
open FsUnitTyped

[<Test>]
let ``direct eps closure`` () =
    let nfa = Automata.emptyNFA |> Automata.addTransition 0 Epsilon 1  |> Automata.addTransition 0 Epsilon 2
    let closure = Automatas.closure nfa 0 Epsilon
    closure |> shouldEqual (Set([0;1;2]))
        
[<Test>]
let ``indirect eps closure`` () =
    let nfa = Automata.emptyNFA
                |> Automata.addTransition 0 Epsilon 1
                |> Automata.addTransition 0 Epsilon 2
                |> Automata.addTransition 2 Epsilon 3
    let closure = Automatas.closure nfa 0 Epsilon
    closure |> shouldEqual (Set([0;1;2;3]))
        
[<Test>]
let ``indirect eps closure 3 levels`` () =
    let nfa = Automata.emptyNFA
                |> Automata.addTransition 0 Epsilon 1
                |> Automata.addTransition 0 Epsilon 2
                |> Automata.addTransition 2 Epsilon 3
                |> Automata.addTransition 3 Epsilon 4
    let closure = Automatas.closure nfa 0 Epsilon
    closure |> shouldEqual (Set([0;1;2;3;4]))
        
[<Test>]
let ``direct a closure`` () =
    let nfa = Automata.emptyNFA |> Automata.addTransition 0 (Char 'a') 1  |> Automata.addTransition 0 (Char 'a') 2
    let closure = Automatas.closure nfa 0 (Char 'a')
    closure |> shouldEqual (Set([1;2]))
        
[<Test>]
let ``indirect a closure`` () =
    let nfa = Automata.emptyNFA |> Automata.addTransition 0 (Char 'a') 1  |> Automata.addTransition 1 (Char 'a') 2
    let closure = Automatas.closure nfa 0 (Char 'a')
    closure |> shouldEqual (Set([1]))
