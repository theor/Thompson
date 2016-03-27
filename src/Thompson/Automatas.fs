namespace Thompson

module Automatas =
    open Regex
    let parse (_:string) : RegEx =
        Val Null

    let opandToNFA (o:Opand) : Automata.NFA option =
        match o with
        | Char _ | Epsilon -> Automata.emptyNFA |> Automata.addTransition 0 o 1 |> Some
//        | Var v -> None
        | _ -> None

    let toNFA (r:RegEx) : Automata.NFA option =
        let rec toNFA_rec (n:int) (r:RegEx) : Automata.NFA option =
            match r with
            | Val v -> Automata.emptyNFA |> Automata.addTransition n v (n+1) |> Automata.addEndState (n+1) |> Some
            | Union(a, b) ->
                let na = toNFA_rec (n+1) a
                let naEnd = (na.Value.ends.Head)
                let nbStart = (naEnd + 1)
                let nb = toNFA_rec nbStart b
                let nbEnd = (nb.Value.ends.Head)

                Automata.emptyNFA
                |> Automata.addSubNfa na.Value
                |> Automata.addSubNfa nb.Value
                |> Automata.addTransition n Epsilon (n+1)
                |> Automata.addTransition n Epsilon (nbStart)
                |> Automata.addTransition naEnd Epsilon (nbEnd + 1)
                |> Automata.addTransition nbEnd Epsilon (nbEnd + 1)
                |> Automata.addEndState (nbEnd + 1)
                |> Some
            | Concat(a, b) ->
                let na = toNFA_rec (n) a
                let nbStart = (na.Value.ends.Head)
                let nb = toNFA_rec nbStart b
                Automata.emptyNFA
                |> Automata.addSubNfa na.Value
                |> Automata.addSubNfa nb.Value
                |> Automata.addEndState (nb.Value.ends.Head)
                |> Some
            (* n -> (nfaStart -> nfaEnd) -> nfaEnd + 1
               n -> nfaEnd + 1
               nfaEnd -> nfaStart
            *)
            | Kleene o -> 
                let nfa = toNFA_rec (n+1) o
                let nEnd = nfa.Value.ends.Head
                Automata.emptyNFA
                |> Automata.addSubNfa nfa.Value
                |> Automata.addTransition n Epsilon (n+1)
                |> Automata.addTransition nEnd Epsilon (nEnd+1)
                |> Automata.addTransition n Epsilon (nEnd+1)
                |> Automata.addTransition nEnd Epsilon (n+1)
                |> Automata.addEndState (nEnd+1)
                |> Some
//            | _ -> None
        toNFA_rec 0 r
        //If the operand is a character c, then our FA has two states, s0 (the start state) and sF (the final, accepting state), and a transition from s0 to sF with label c.
        //If the operand is epsilon, then our FA has two states, s0 (the start state) and sF (the final, accepting state), and an epsilon transition from s0 to sF.
        //If the operand is null, then our FA has two states, s0 (the start state) and sF (the final, accepting state), and no transitions.
        
    let rec closure(fsm:Automata.FSM<_>)(state:_)(op:Opand) : Set<_> =
        
        let newStates =
            (if op = Epsilon then [state] else []) @
            Automata.stepState fsm op state |> Set
        if op = Epsilon then
            Set.unionMany (newStates :: (Seq.choose (fun s -> if s = state then None else Some(closure fsm s op |> Set)) newStates |> Seq.toList))
        else
            Set(newStates)

    let epsilonRemoval(enfa:Automata.FSM<_>) : Automata.FSM<_> option =
//        let _m = enfa.transitions |> Map.filter (fun f (c,t) -> true)
        None

    let toDFA (nfa:Automata.NFA) : Automata.DFA option =
        let rec toDFA_rec (nfa:Automata.NFA) (states:Set<int>) (dfa:Automata.DFA) =
            let allTransitions =
                states |> Seq.collect (fun i -> Automata.getTransitionsFrom i nfa)
                       |> Seq.filter (fun (c,_) -> c <> Epsilon) // use closure on eps
            let opandToTarget = allTransitions |> Seq.groupBy (fun (c,_) -> c)
            let fold dfa (op, ts) =
                let nextStates = ts |> Seq.map snd
                                    //|> Seq.collect (fun ns -> closure nfa ns Epsilon)
                                    |> Set
                let dfa = dfa |> Automata.addTransition states op nextStates
                              |> toDFA_rec nfa nextStates
                if nextStates |> Automata.isDone nfa then
                    dfa |> Automata.addEndState nextStates
                else
                    dfa
            opandToTarget |> Seq.fold fold dfa
//            let closures = states |> Seq.map 
        let init = closure nfa nfa.start Epsilon
        let dfa = Automata.emptyDFA
                  |> Automata.setInitState init
//                  |> Automata.addEndState init
                  |> toDFA_rec nfa init
        if dfa.ends.IsEmpty then
            dfa |> Automata.addEndState init |> Some
        else
            dfa |> Some
