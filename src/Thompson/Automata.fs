namespace Thompson

module Automata =
    type State = int
    open Regex
    open Microsoft.FSharp.Collections
//    let map = Map.ofSeq [(1,1)]
    type Transition = Opand * State
    type NFA = { transitions: Map<State,Transition list>
                 start : State
                 ends: State list }
//        with
//            override x.ToString() = sprintf "%A" x

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

    let addSubNfa (sub:NFA) (n:NFA) : NFA =
        sub.transitions
        |> Map.toList
        |> List.collect (fun (k, t) -> t |> List.map (fun x -> (k,x)))
        |> List.fold(fun s (fromS,(op,toS)) -> s |> addTransition fromS op toS) n

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
