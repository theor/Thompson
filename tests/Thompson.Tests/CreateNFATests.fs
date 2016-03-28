module CreateNFATests

open Thompson
open Thompson.Regex
open NUnit.Framework
open FsUnitTyped

//    [<Test>]
//    let ``add state to nfa`` () =
//        let nfa = Automata.emptyNFA |> Automata.addState 1 |> Automata.addState 2
//        Assert.AreEqual(2, nfa |> Automata.getStateCount)
//
//    [<Test>]
//    let ``add state twice to nfa`` () =
//        let nfa = Automata.emptyNFA |> Automata.addState 1 |> Automata.addState 1
//        Assert.AreEqual(1, nfa |> Automata.getStateCount)

[<Test>]
let ``add transition to nfa`` () =
    let nfa = Automata.emptyNFA |> Automata.addTransition 1 (Char 'a') 2
    Assert.AreEqual(1, 1 |> Automata.getTransitionsCount nfa)

[<Test>]
let ``add 2 transitions to nfa`` () =
    let nfa = Automata.emptyNFA
                |> Automata.addTransition 1 (Char 'a') 2
                |> Automata.addTransition 1 (Char 'b') 2
    Assert.AreEqual(2, 1 |> Automata.getTransitionsCount nfa)

[<Test>]
let ``is done`` () =
    let nfa = Automata.emptyNFA |> Automata.addEndState 42
    Assert.IsTrue(Automata.isDone nfa [42])

[<Test>]
let ``is not done`` () =
    let nfa = Automata.emptyNFA |> Automata.addEndState 42
    Assert.IsFalse(Automata.isDone nfa [7])

