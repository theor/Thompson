namespace Thompson

module Automata =
    open Regex
    open Microsoft.FSharp.Collections
//    let map = Map.ofSeq [(1,1)]
    type FSM<'T when 'T:comparison> = { transitions: Map<'T * Opand,'T list>
                                        start : 'T
                                        ends: Set<'T> }
    type NFA = FSM<int>
    type DFA = FSM<Set<int>>
//        with
//            override x.ToString() = sprintf "%A" x
    let emptyNFA : NFA =
        { transitions = Map.empty
          start = 0
          ends = Set([]) }
    let emptyDFA : DFA =
        { transitions = Map.empty
          start = Set([])
          ends = Set([]) }

//    let getStateCount(n:FSM<_>) = n.transitions.Count

//    let addState<'T when 'T:comparison> (s:'T) (n:FSM<'T>) : FSM<'T> =
//        n

    let setInitState<'T when 'T:comparison> (s:'T) (n:FSM<'T>) : FSM<'T> =
        { n with start = s }

    let addTransition<'T when 'T:comparison> (from:'T) (t:Opand) (dest:'T) (n:FSM<'T>) : FSM<'T> =
        let l = match n.transitions |> Map.tryFind (from,t) with
                | None -> []
                | Some l -> l
        { n with transitions = n.transitions |> Map.add (from,t) (dest :: l) }

    let addSubNfa (sub:NFA) (n:NFA) : NFA =
        sub.transitions
        |> Map.toList
        |> List.collect (fun (k, t) -> t |> List.map (fun x -> (k,x)))
        |> List.fold(fun s ((fromS,op),toS) -> s |> addTransition fromS op toS) n

    let addEndState (s) (n:FSM<_>) : FSM<_> =
        { n with ends = n.ends |> Set.add s }
        
    let getTransitionsFrom<'T when 'T:comparison> (n:FSM<'T>) (from:'T) =
        n.transitions |> Map.filter (fun (f,_) _ -> f = from) |> Map.toList |> List.collect (fun ((_,o),tl) -> tl |> List.map (fun t -> (o,t)))

    let getTransitionsCount<'T when 'T:comparison> (n:FSM<'T>) (from:'T) = getTransitionsFrom n from |> List.length

    let isDone (n:FSM<_>) (states:_ seq) =
        not << Set.isEmpty <| Set.intersect n.ends (Set.ofSeq states)

    let stepState (n:FSM<_>) (o:Opand) (curState:_) : _ list =
        match n.transitions |> Map.tryFind (curState,o) with
        | None -> curState :: []
        | Some l -> l

    let step (n:FSM<_>) (o:Opand) (curStates:_ list) : _ list =
        curStates |> List.collect (stepState n o)

    let stepDFA (d:DFA) (o:Opand) (s:_) : _ option =
        match d.transitions |> Map.tryFind (s,o) with
        | None -> None
        | Some (x :: []) -> Some x
        | Some l -> failwithf "Non deterministic, transition %A,%A has multiple target states: %A" s o l

    let isMatchDFA (n:DFA) (s:string) : bool =
        let folded = s |> Seq.fold (fun s c -> s |> Option.bind (stepDFA n (Char c))) (Some n.start)
        match folded with
        | None -> false
        | Some(s) ->  [s] |> isDone n
        
    let isMatch (n:FSM<_>) (s:string) : bool =
        s |> Seq.fold (fun s c -> step n (Char c) s) (n.start :: []) |> isDone n
