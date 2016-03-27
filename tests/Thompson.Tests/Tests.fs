module Thompson.Tests

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
        Automata.getTransitionsCount (Set[1;2])

module CreateNFA =

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
        Assert.AreEqual(1, nfa |> Automata.getTransitionsCount 1)

    [<Test>]
    let ``add 2 transitions to nfa`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.addTransition 1 (Char 'a') 2
                  |> Automata.addTransition 1 (Char 'b') 2
        Assert.AreEqual(2, nfa |> Automata.getTransitionsCount 1)

    [<Test>]
    let ``is done`` () =
        let nfa = Automata.emptyNFA |> Automata.addEndState 42
        Assert.IsTrue(Automata.isDone nfa [42])

    [<Test>]
    let ``is not done`` () =
        let nfa = Automata.emptyNFA |> Automata.addEndState 42
        Assert.IsFalse(Automata.isDone nfa [7])

module Closure =
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

module EpsilonRemoval =
    let test nfa expNfa =
        nfa |> Automatas.epsilonRemoval |> shouldEqual (Some(expNfa))
    [<Test>]
    let ``a*`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.setInitState 0
                  |> Automata.addTransition 0 (Char 'a') 0
                  |> Automata.addTransition 0 Epsilon 1
                  |> Automata.addEndState 1
        let expNfa = Automata.emptyNFA
                  |> Automata.setInitState 0
                  |> Automata.addTransition 0 (Char 'a') 0
                  |> Automata.addTransition 0 (Char 'a') 1
                  |> Automata.addEndState 0
                  |> Automata.addEndState 1
        test nfa expNfa
    [<Test>]
    let ``ab*`` () =
        let nfa = Automata.emptyNFA
                  |> Automata.addTransition 0 (Char 'a') 1
                  |> Automata.addTransition 1 (Char 'b') 1
                  |> Automata.addTransition 1 Epsilon 2
                  |> Automata.addEndState 2
        let expNfa = Automata.emptyNFA
                  |> Automata.addTransition 0 (Char 'a') 1
                  |> Automata.addTransition 1 (Char 'b') 1
                  |> Automata.addTransition 1 (Char 'b') 2
                  |> Automata.addTransition 0 (Char 'a') 2
                  |> Automata.addEndState 2
        test nfa expNfa
    
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

module RegexToNFA =
    let test (o:Op) (expNfa:Automata.NFA) =
        let nfa = Automatas.toNFA o
        Assert.IsTrue(Option.isSome nfa, "No automata produced")
        (Option.get nfa) |> shouldEqual expNfa

    [<Test>]
    let ``a`` () =
        let expected = Automata.emptyNFA
                       |> Automata.addTransition 0 (Char 'a') 1
                       |> Automata.addEndState 1
        test (Val(Char 'a')) expected
        
    [<Test>]
    let ``a*`` () =
        let expected = Automata.emptyNFA
                       |> Automata.addTransition 0 Epsilon 1
                       |> Automata.addTransition 1 (Char 'a') 2
                       |> Automata.addTransition 2 Epsilon 3
                       |> Automata.addTransition 2 Epsilon 1
                       |> Automata.addTransition 0 Epsilon 3
                       |> Automata.addEndState 3
        test (Kleene(Val(Char 'a'))) expected
        
    [<Test>]
    let ``ab``() =
        let expected = Automata.emptyNFA
                       |> Automata.addTransition 0 (Char 'a') 1
                       |> Automata.addTransition 1 (Char 'b') 2
                       |> Automata.addEndState 2
        test (Concat(Val(Char 'a'),Val(Char 'b'))) expected
        
    [<Test>]
    let ``a|b``() =
        let expected = Automata.emptyNFA
                       |> Automata.addTransition 0 Epsilon 1
                       |> Automata.addTransition 1 (Char 'a') 2
                       |> Automata.addTransition 2 Epsilon 5
                       |> Automata.addTransition 0 Epsilon 3
                       |> Automata.addTransition 3 (Char 'b') 4
                       |> Automata.addTransition 4 Epsilon 5
                       |> Automata.addEndState 5
        test (Union(Val(Char 'a'),Val(Char 'b'))) expected

    [<Test>]
    let ``ab|c``() =
        let expected = Automata.emptyNFA
                       |> Automata.addTransition 0 Epsilon 1
                       |> Automata.addTransition 1 (Char 'a') 2
                       |> Automata.addTransition 2 (Char 'b') 3
                       |> Automata.addTransition 3 Epsilon 6

                       |> Automata.addTransition 0 Epsilon 4
                       |> Automata.addTransition 4 (Char 'c') 5
                       |> Automata.addTransition 5 Epsilon 6
                       |> Automata.addEndState 6
        test (Union(Concat(Val(Char 'a'),Val(Char 'b')), Val(Char 'c'))) expected
        
    [<Test>]
    let ``a|bc``() =
        let expected = Automata.emptyNFA
                       |> Automata.addTransition 0 Epsilon 1
                       |> Automata.addTransition 1 (Char 'a') 2
                       |> Automata.addTransition 2 Epsilon 6

                       |> Automata.addTransition 0 Epsilon 3
                       |> Automata.addTransition 3 (Char 'b') 4
                       |> Automata.addTransition 4 (Char 'c') 5
                       |> Automata.addTransition 5 Epsilon 6
                       |> Automata.addEndState 6
        test (Union(Val(Char 'a'), Concat(Val(Char 'b'),Val(Char 'c')))) expected
        
    [<Test>]
    let ``a|c``() =
        let expected = Automata.emptyNFA
                       |> Automata.addTransition 0 Epsilon 1
                       |> Automata.addTransition 1 (Char 'a') 2
                       |> Automata.addTransition 2 Epsilon 5

                       |> Automata.addTransition 0 Epsilon 3
                       |> Automata.addTransition 3 (Char 'c') 4
                       |> Automata.addTransition 4 Epsilon 5
                       |> Automata.addEndState 5
        test (Union(Val(Char 'a'), Val(Char 'c'))) expected

module ParserTests =
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



        