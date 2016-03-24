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
    type Transition = Opand * State
    type NFA = { transitions: Map<State,Transition list>
                 start : State
                 ends: State list }

    let empty : NFA =
        { transitions = Map.empty
          start = 0
          ends = [] }

    let getStateCount (n:NFA) = n.transitions.Count

    let addState (s:State) (n:NFA) : NFA =
        if n.transitions |> Map.containsKey s then n
        else { n with transitions = n.transitions |> Map.add s [] }

    let addTransition (from:State) (t:Opand) (dest:State) (n:NFA) : NFA =
        let l = match n.transitions |> Map.tryFind from with
                | None -> []
                | Some l -> l
        { n with transitions = n.transitions |> Map.add from ((t,dest) :: l) }

    let addEndState (s:State) (n:NFA) : NFA =
        { n with ends = s :: n.ends }

    let getTransitionsCount (from:State) (n:NFA) =
        match n.transitions |> Map.tryFind from with
        | None -> 0
        | Some l -> List.length l

    let isDone (n:NFA) (states:State list) =
        not << Set.isEmpty <| Set.intersect (Set.ofList n.ends) (Set.ofList states)

    let stepState (n:NFA) (o:Opand) (curState:State) : State list =
        match n.transitions |> Map.tryFind curState with
        | None -> curState :: []
        | Some l -> l |> List.choose (fun (c,s) -> if c = o then Some s else None)

    let step (n:NFA) (o:Opand) (curStates:State list) : State list =
        curStates |> List.collect (stepState n o)

    let isMatch (n:NFA) (s:string) : bool =
        s |> Seq.fold (fun s c -> step n (Char c) s) (n.start :: []) |> isDone n

type Automata = RegEx

module Automatas =
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

module Parser = 
    open FParsec
    let parse s =
        let p = pfloat
        FParsec.CharParsers.run p s