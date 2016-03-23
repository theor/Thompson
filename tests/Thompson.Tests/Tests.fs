module Thompson.Tests

open Thompson
open NUnit.Framework

module CreateNFA =
    [<Test>]
    let ``hello returns 42`` () =
      let result = Library.hello 42
      printfn "%i" result
      Assert.AreEqual(42,result)

    [<Test>]
    let ``add state to nfa`` () =
        let nfa = NFA.empty |> NFA.addState 1 |> NFA.addState 2
        Assert.AreEqual(2, nfa |> NFA.getStateCount)

    [<Test>]
    let ``add state twice to nfa`` () =
        let nfa = NFA.empty |> NFA.addState 1 |> NFA.addState 1
        Assert.AreEqual(1, nfa |> NFA.getStateCount)

    [<Test>]
    let ``add transition to nfa`` () =
        let nfa = NFA.empty |> NFA.addState 1 |> NFA.addState 2 |> NFA.addTransition 1 (Char 'a') 2
        Assert.AreEqual(1, nfa |> NFA.getTransitionsCount 1)

    [<Test>]
    let ``add 2 transitions to nfa`` () =
        let nfa = NFA.empty |> NFA.addState 1 |> NFA.addState 2
                  |> NFA.addTransition 1 (Char 'a') 2
                  |> NFA.addTransition 1 (Char 'b') 2
        Assert.AreEqual(2, nfa |> NFA.getTransitionsCount 1)

    [<Test>]
    let ``is done`` () =
        let nfa = NFA.empty |> NFA.addEndState 42
        Assert.IsTrue(NFA.isDone nfa [42])

    [<Test>]
    let ``is not done`` () =
        let nfa = NFA.empty |> NFA.addEndState 42
        Assert.IsFalse(NFA.isDone nfa [7])

module NFAStep =

    [<Test>]
    let ``step char transition should change state`` () =
        let nfa = NFA.empty |> NFA.addState 0 |> NFA.addState 1
                  |> NFA.addTransition 0 (Char 'a') 1
                  |> NFA.addTransition 0 (Char 'b') 2
        let init = nfa.start
        let stepped = NFA.stepState nfa (Char 'a') init
        Assert.AreEqual([ 1 ], stepped)

    [<Test>]
    let ``step 2nd char transition should change state`` () =
        let nfa = NFA.empty |> NFA.addState 0 |> NFA.addState 1
                  |> NFA.addTransition 0 (Char 'a') 1
                  |> NFA.addTransition 0 (Char 'b') 2
        let init = nfa.start
        let stepped = NFA.stepState nfa (Char 'b') init
        Assert.AreEqual([ 2 ], stepped)

    [<Test>]
    let ``step epsilon transition should change state`` () =
        let nfa = NFA.empty |> NFA.addState 0 |> NFA.addState 1
                  |> NFA.addTransition 0 (Char 'a') 1
                  |> NFA.addTransition 0 Epsilon 2
        let init = nfa.start
        let stepped = NFA.stepState nfa Epsilon init
        Assert.AreEqual([ 2 ], stepped)

    [<Test>]
    let ``step two epsilon transition should change state`` () =
        let nfa = NFA.empty |> NFA.addState 0 |> NFA.addState 1
                  |> NFA.addTransition 0 Epsilon 1
                  |> NFA.addTransition 0 Epsilon 2
        let init = nfa.start
        let stepped = NFA.stepState nfa Epsilon init
        Assert.AreEqual([ 1; 2 ], stepped |> List.sort)

    [<Test>]
    let ``step two epsilon transition twice should yield four states`` () =
        let nfa = NFA.empty |> NFA.addState 0 |> NFA.addState 1
                  |> NFA.addTransition 0 Epsilon 1
                  |> NFA.addTransition 1 Epsilon 10
                  |> NFA.addTransition 1 Epsilon 11
                  |> NFA.addTransition 0 Epsilon 2
                  |> NFA.addTransition 2 Epsilon 20
                  |> NFA.addTransition 2 Epsilon 21
        let init = nfa.start
        let stepped = init :: [] |> NFA.step nfa Epsilon |>  NFA.step nfa Epsilon
        Assert.AreEqual([ 10; 11; 20; 21 ], stepped |> List.sort)

module NFAIsMatch =
    [<Test>]
    let ``match abc`` () =
        let n = NFA.empty |> NFA.addTransition 0 (Char 'a') 1
                          |> NFA.addTransition 1 (Char 'b') 2
                          |> NFA.addTransition 2 (Char 'c') 3
                          |> NFA.addEndState 3
        Assert.IsTrue(NFA.isMatch n "abc")

    [<Test>]
    [<TestCase("a")>]
    [<TestCase("")>]
    let ``match a|epsilon`` (s:string) =
        let n = NFA.empty |> NFA.addTransition 0 (Char 'a') 1
                          |> NFA.addTransition 0 Epsilon 1
                          |> NFA.addEndState 1
        Assert.IsTrue(NFA.isMatch n s)