module Thompson.Tests

open Thompson
open NUnit.Framework

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
    let nfa = NFA.empty |> NFA.addState 1 |> NFA.addState 2 |> NFA.addTransition 1 'a' 2
    Assert.AreEqual(1, nfa |> NFA.getTransitionsCount 1)
[<Test>]
let ``add 2 transitions to nfa`` () =
    let nfa = NFA.empty |> NFA.addState 1 |> NFA.addState 2
              |> NFA.addTransition 1 'a' 2
              |> NFA.addTransition 1 'b' 2
    Assert.AreEqual(2, nfa |> NFA.getTransitionsCount 1)