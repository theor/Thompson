namespace Thompson

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Library = 
  
  /// Returns 42
  ///
  /// ## Parameters
  ///  - `num` - whatever
  let hello num = 42

type Opand =
    | Char of char
    | Var of string
    | Epsilon
    | Null
type Op =
    | Union of Opand * Opand
    | Concat of Opand * Opand
    | Kleene of Opand
    | Val of Opand
type RegEx = Op

type State = int
module NFA =
    open Microsoft.FSharp.Collections
//    let map = Map.ofSeq [(1,1)]
    type Transition = char * State
    type NFA = Map<State,Transition list>

    let empty : NFA = Map.empty

    let getStateCount (n:NFA) = n.Count
    
    let addState (s:State) (n:NFA) : NFA =
        if n |> Map.containsKey s then n
        else n |> Map.add s []
    
    let addTransition (from:State) (t:char) (dest:State) (n:NFA) : NFA =
        let l = match n |> Map.tryFind from with
                | None -> []
                | Some l -> l
        n |> Map.add from ((t,dest) :: l)

    let getTransitionsCount (from:State) (n:NFA) =
        match n |> Map.tryFind from with
        | None -> 0
        | Some l -> List.length l

type Automata = RegEx

module Parser =
    let parse (s:string) : RegEx =
        Val Null

    let opandToNFA (o:Opand) : Automata option =
        match o with
        | Char c -> None
        | Var v -> None
        | Epsilon -> None
        | _ -> None

    let toNFA (r:RegEx) : Automata option =
        match r with
        | Val v -> opandToNFA v
        | Union(a, b) -> None// (opandToNFA a) (opandToNFA b)
        | Concat(a, b) -> None// (opandToNFA a) (opandToNFA b)
        | Kleene o -> None// (opandToNFA o)
        | _ -> None

        //If the operand is a character c, then our FA has two states, s0 (the start state) and sF (the final, accepting state), and a transition from s0 to sF with label c.
        //If the operand is epsilon, then our FA has two states, s0 (the start state) and sF (the final, accepting state), and an epsilon transition from s0 to sF.
        //If the operand is null, then our FA has two states, s0 (the start state) and sF (the final, accepting state), and no transitions.