namespace Thompson

module Automata =
    open Regex
    open Microsoft.FSharp.Collections
//    let map = Map.ofSeq [(1,1)]
    type Transition<'T when 'T:comparison> = Opand * 'T
    type FSM<'T when 'T:comparison> = { transitions: Map<'T,Transition<'T> list>
                                        start : 'T
                                        ends: 'T list }
    type NFA = FSM<int>
    type DFA = FSM<Set<int>>
//        with
//            override x.ToString() = sprintf "%A" x
    let emptyNFA : NFA =
        { transitions = Map.empty
          start = 0
          ends = [] }
    let emptyDFA : DFA =
        { transitions = Map.empty
          start = Set([])
          ends = [] }

    let getStateCount(n:FSM<_>) = n.transitions.Count

    let addState<'T when 'T:comparison> (s:'T) (n:FSM<'T>) : FSM<'T> =
        if n.transitions |> Map.containsKey s then n
        else { n with transitions = n.transitions |> Map.add s [] }

    let setInitState<'T when 'T:comparison> (s:'T) (n:FSM<'T>) : FSM<'T> =
        { n with start = s }

    let addTransition<'T when 'T:comparison> (from:'T) (t:Opand) (dest:'T) (n:FSM<'T>) : FSM<'T> =
        let l = match n.transitions |> Map.tryFind from with
                | None -> []
                | Some l -> l
        { n with transitions = n.transitions |> Map.add from ((t,dest) :: l) }

    let addSubNfa (sub:NFA) (n:NFA) : NFA =
        sub.transitions
        |> Map.toList
        |> List.collect (fun (k, t) -> t |> List.map (fun x -> (k,x)))
        |> List.fold(fun s (fromS,(op,toS)) -> s |> addTransition fromS op toS) n

    let addEndState (s) (n:FSM<_>) : FSM<_> =
        { n with ends = s :: n.ends }

    let getTransitionsCount<'T when 'T:comparison> (from:'T) (n:FSM<'T>) =
        match n.transitions |> Map.tryFind from with
        | None -> 0
        | Some l -> List.length l

    let isDone (n:FSM<_>) (states:_ seq) =
        not << Set.isEmpty <| Set.intersect (Set.ofList n.ends) (Set.ofSeq states)

    let stepState (n:FSM<_>) (o:Opand) (curState:_) : _ list =
        match n.transitions |> Map.tryFind curState with
        | None -> curState :: []
        | Some l -> l |> List.choose (fun (c,s) -> if c = o then Some s else None)

    let step (n:FSM<_>) (o:Opand) (curStates:_ list) : _ list =
        curStates |> List.collect (stepState n o)

    let isMatch (n:FSM<_>) (s:string) : bool =
        s |> Seq.fold (fun s c -> step n (Char c) s) (n.start :: []) |> isDone n
