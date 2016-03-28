module RegexToNFATests

open Thompson
open Thompson.Regex
open NUnit.Framework
open FsUnitTyped

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
        