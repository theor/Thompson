module Thompson.Tests

open Thompson
open Thompson.Regex
open NUnit.Framework
open FsUnitTyped

module CreateNFA =

    [<Test>]
    let ``add state to nfa`` () =
        let nfa = Automata.empty |> Automata.addState 1 |> Automata.addState 2
        Assert.AreEqual(2, nfa |> Automata.getStateCount)

    [<Test>]
    let ``add state twice to nfa`` () =
        let nfa = Automata.empty |> Automata.addState 1 |> Automata.addState 1
        Assert.AreEqual(1, nfa |> Automata.getStateCount)

    [<Test>]
    let ``add transition to nfa`` () =
        let nfa = Automata.empty |> Automata.addState 1 |> Automata.addState 2 |> Automata.addTransition 1 (Char 'a') 2
        Assert.AreEqual(1, nfa |> Automata.getTransitionsCount 1)

    [<Test>]
    let ``add 2 transitions to nfa`` () =
        let nfa = Automata.empty |> Automata.addState 1 |> Automata.addState 2
                  |> Automata.addTransition 1 (Char 'a') 2
                  |> Automata.addTransition 1 (Char 'b') 2
        Assert.AreEqual(2, nfa |> Automata.getTransitionsCount 1)

    [<Test>]
    let ``is done`` () =
        let nfa = Automata.empty |> Automata.addEndState 42
        Assert.IsTrue(Automata.isDone nfa [42])

    [<Test>]
    let ``is not done`` () =
        let nfa = Automata.empty |> Automata.addEndState 42
        Assert.IsFalse(Automata.isDone nfa [7])

module NFAStep =

    [<Test>]
    let ``step char transition should change state`` () =
        let nfa = Automata.empty |> Automata.addState 0 |> Automata.addState 1
                  |> Automata.addTransition 0 (Char 'a') 1
                  |> Automata.addTransition 0 (Char 'b') 2
        let init = nfa.start
        let stepped = Automata.stepState nfa (Char 'a') init
        Assert.AreEqual([ 1 ], stepped)

    [<Test>]
    let ``step 2nd char transition should change state`` () =
        let nfa = Automata.empty |> Automata.addState 0 |> Automata.addState 1
                  |> Automata.addTransition 0 (Char 'a') 1
                  |> Automata.addTransition 0 (Char 'b') 2
        let init = nfa.start
        let stepped = Automata.stepState nfa (Char 'b') init
        Assert.AreEqual([ 2 ], stepped)

    [<Test>]
    let ``step epsilon transition should change state`` () =
        let nfa = Automata.empty |> Automata.addState 0 |> Automata.addState 1
                  |> Automata.addTransition 0 (Char 'a') 1
                  |> Automata.addTransition 0 Epsilon 2
        let init = nfa.start
        let stepped = Automata.stepState nfa Epsilon init
        Assert.AreEqual([ 2 ], stepped)

    [<Test>]
    let ``step two epsilon transition should change state`` () =
        let nfa = Automata.empty |> Automata.addState 0 |> Automata.addState 1
                  |> Automata.addTransition 0 Epsilon 1
                  |> Automata.addTransition 0 Epsilon 2
        let init = nfa.start
        let stepped = Automata.stepState nfa Epsilon init
        Assert.AreEqual([ 1; 2 ], stepped |> List.sort)

    [<Test>]
    let ``step two epsilon transition twice should yield four states`` () =
        let nfa = Automata.empty |> Automata.addState 0 |> Automata.addState 1
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
        let n = Automata.empty |> Automata.addTransition 0 (Char 'a') 1
                          |> Automata.addTransition 1 (Char 'b') 2
                          |> Automata.addTransition 2 (Char 'c') 3
                          |> Automata.addEndState 3
        Assert.IsTrue(Automata.isMatch n "abc")

    [<Test>]
    [<TestCase("a")>]
//    [<TestCase("")>]
    let ``match a|epsilon`` (s:string) =
        let n = Automata.empty |> Automata.addTransition 0 (Char 'a') 1
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
        let expected = Automata.empty
                       |> Automata.addTransition 0 (Char 'a') 1
                       |> Automata.addEndState 1
        test (Val(Char 'a')) expected
        
    [<Test>]
    let ``a*`` () =
        let expected = Automata.empty
                       |> Automata.addTransition 0 Epsilon 1
                       |> Automata.addTransition 1 (Char 'a') 2
                       |> Automata.addTransition 2 Epsilon 3
                       |> Automata.addTransition 2 Epsilon 1
                       |> Automata.addTransition 0 Epsilon 3
                       |> Automata.addEndState 3
        test (Kleene(Val(Char 'a'))) expected
        
    [<Test>]
    let ``ab``() =
        let expected = Automata.empty
                       |> Automata.addTransition 0 (Char 'a') 1
                       |> Automata.addTransition 1 (Char 'b') 2
                       |> Automata.addEndState 2
        test (Concat(Val(Char 'a'),Val(Char 'b'))) expected
        
    [<Test>]
    let ``a|b``() =
        let expected = Automata.empty
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
        let expected = Automata.empty
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
        let expected = Automata.empty
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
        let expected = Automata.empty
                       |> Automata.addTransition 0 Epsilon 1
                       |> Automata.addTransition 1 (Char 'a') 2
                       |> Automata.addTransition 2 Epsilon 6

                       |> Automata.addTransition 0 Epsilon 3
                       |> Automata.addTransition 3 (Char 'b') 4
                       |> Automata.addTransition 4 (Char 'c') 5
                       |> Automata.addTransition 5 Epsilon 6
                       |> Automata.addEndState 6
        test (Union(Val(Char 'a'), Val(Char 'c'))) expected

module ParserTests =
    open FParsec
    [<Test>]
    let ``parse char`` () =
        let x = Parser.parse "a*|b|c"
        printfn "%A" x
        match x with
        | Success(a,b,c) -> printfn "%A" a
        | _ -> Assert.Fail()
        