namespace Thompson.Tests

open Thompson
open Thompson.Regex
open NUnit.Framework
open FsUnitTyped

module CreateDFA =
//    [<Test>]
//    let ``add state to dfa`` () =
//        let nfa = Automata.emptyDFA |> Automata.addState (Set([1;2])) |> Automata.addState (Set([1;3]))
//        Assert.AreEqual(2, nfa |> Automata.getStateCount)
    [<Test>]
    let ``add transitions to dfa`` () =
        let nfa = Automata.emptyDFA |> Automata.addTransition (Set([1;2])) (Char 'a') (Set([1;3]))
//        Assert.AreEqual(2, nfa |> Automata.getStateCount)
        1 |> shouldEqual (Automata.getTransitionsCount nfa (Set[1;2]))
    
module ToDFA =
    let test nfa expDfa =
        nfa |> Automatas.toDFA |> shouldEqual (Some(expDfa))
        
    [<Test>]
    let ``one transition dfa`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.addTransition 0 Epsilon 1
                  |> Automata.addEndState 1
        let s = Set([0;1])
        let expDfa = Automata.emptyDFA
                     |> Automata.setInitState s
                     |> Automata.addEndState s
        test nfa expDfa

    [<Test>]
    let ``2 transition dfa`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.addTransition 0 Epsilon 1
                  |> Automata.addTransition 0 (Char 'a') 2
                  |> Automata.addTransition 1 (Char 'a') 3
                  |> Automata.addEndState 2
        let s01 = Set([0;1])
        let s23 = Set([2;3])
        let expDfa = Automata.emptyDFA
                     |> Automata.addTransition s01 (Char 'a') s23
                     |> Automata.setInitState s01
                     |> Automata.addEndState s23
        test nfa expDfa

module NFAStep =

    [<Test>]
    let ``step char transition should change state`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.addTransition 0 (Char 'a') 1
                  |> Automata.addTransition 0 (Char 'b') 2
        let init = nfa.start
        let stepped = Automata.stepState nfa (Char 'a') init
        Assert.AreEqual([ 1 ], stepped)

    [<Test>]
    let ``step 2nd char transition should change state`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.addTransition 0 (Char 'a') 1
                  |> Automata.addTransition 0 (Char 'b') 2
        let init = nfa.start
        let stepped = Automata.stepState nfa (Char 'b') init
        Assert.AreEqual([ 2 ], stepped)

    [<Test>]
    let ``step epsilon transition should change state`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.addTransition 0 (Char 'a') 1
                  |> Automata.addTransition 0 Epsilon 2
        let init = nfa.start
        let stepped = Automata.stepState nfa Epsilon init
        Assert.AreEqual([ 2 ], stepped)

    [<Test>]
    let ``step two epsilon transition should change state`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.addTransition 0 Epsilon 1
                  |> Automata.addTransition 0 Epsilon 2
        let init = nfa.start
        let stepped = Automata.stepState nfa Epsilon init
        Assert.AreEqual([ 1; 2 ], stepped |> List.sort)

    [<Test>]
    let ``step two epsilon transition twice should yield four states`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.addTransition 0 Epsilon 1
                  |> Automata.addTransition 1 Epsilon 10
                  |> Automata.addTransition 1 Epsilon 11
                  |> Automata.addTransition 0 Epsilon 2
                  |> Automata.addTransition 2 Epsilon 20
                  |> Automata.addTransition 2 Epsilon 21
        let init = nfa.start
        let stepped = init :: [] |> Automata.step nfa Epsilon |>  Automata.step nfa Epsilon
        Assert.AreEqual([ 10; 11; 20; 21 ], stepped |> List.sort)

module NFAIsMatch =
    [<Test>]
    let ``match abc`` () =
        let n = Automata.emptyNFA |> Automata.addTransition 0 (Char 'a') 1
                          |> Automata.addTransition 1 (Char 'b') 2
                          |> Automata.addTransition 2 (Char 'c') 3
                          |> Automata.addEndState 3
        Assert.IsTrue(Automata.isMatch n "abc")

    [<Test>]
    [<TestCase("a")>]
//    [<TestCase("")>]
    let ``match a|epsilon`` (s:string) =
        let n = Automata.emptyNFA |> Automata.addTransition 0 (Char 'a') 1
                          |> Automata.addTransition 0 Epsilon 1
                          |> Automata.addEndState 1
        Assert.IsTrue(Automata.isMatch n s)

