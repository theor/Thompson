namespace Thompson

module Automatas =
    open Regex
    let parse (_:string) : RegExAst =
        Val Null

    let opandToNFA (o:Opand) : Automata.NFA option =
        match o with
        | Char _ | Epsilon -> Automata.emptyNFA |> Automata.addTransition 0 o 1 |> Some
//        | Var v -> None
        | _ -> None

    let toNFA (r:RegExAst) : Automata.NFA option =
        let rec toNFA_rec (n:int) (r:RegExAst) : Automata.NFA option =
            match r with
            | Val v -> Automata.emptyNFA |> Automata.addTransition n v (n+1) |> Automata.addEndState (n+1) |> Some
            | Union(a, b) ->
                let na = toNFA_rec (n+1) a
                let naEnd = (na.Value.ends |> Set.maxElement)
                let nbStart = (naEnd + 1)
                let nb = toNFA_rec nbStart b
                let nbEnd = (nb.Value.ends |> Set.maxElement)

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
                let nbStart = (na.Value.ends |> Set.maxElement)
                let nb = toNFA_rec nbStart b
                Automata.emptyNFA
                |> Automata.addSubNfa na.Value
                |> Automata.addSubNfa nb.Value
                |> Automata.addEndState (nb.Value.ends |> Set.maxElement)
                |> Some
            (* n -> (nfaStart -> nfaEnd) -> nfaEnd + 1
               n -> nfaEnd + 1
               nfaEnd -> nfaStart
            *)
            | Kleene o -> 
                let nfa = toNFA_rec (n+1) o
                let nEnd = nfa.Value.ends |> Set.maxElement
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

    let epsilonRemoval(enfa:Automata.NFA) : Automata.FSM<_> option =
//        let rec epsilonRemoval_rec(enfa:Automata.NFA) statesDone (states:Set<_>) nfa : Automata.FSM<_> option =
//            printfn "states: %A done %A" states statesDone
//            let allTransitions =
//                states |> Seq.collect (fun i -> Automata.getTransitionsFrom i enfa)
//                       |> Seq.filter (fun (c,_) -> c <> Epsilon) // use closure on eps
//                       |> Seq.groupBy fst
//                       |> Seq.map (fun (op, opStateList) -> (op, opStateList |> Seq.map snd |> Seq.map (fun l -> closure enfa l op) |> Set.unionMany))
//            let newNfa = allTransitions
//                         |> Seq.fold (fun a (op,toStates) -> a |> Automata.addTransition states op toStates ) nfa
//            let newNfaRec,_ = allTransitions
//                            |> Seq.filter (fun (_,toStates) -> not <| Set.contains toStates statesDone)
//                            |> Seq.fold (fun (a,set) (_,toStates) ->
//                                let newSet = (set |> Set.add toStates)
//                                (epsilonRemoval_rec enfa newSet toStates a |> Option.get),newSet) (newNfa,statesDone)
//
//            newNfaRec |> Some
        let folder (states:Set<_>) a (op,set) =
            let a = a |> Automata.addTransition states op set
            if Automata.isDone enfa set then
                a |> Automata.addEndState set
            else a
        let rec epsilonRemoval_rec (sdone:Set<_>) (enfa:Automata.NFA) (states:Set<_>) nfa : Automata.FSM<_> option =
            let findReachables op =
                let tos = states |> Seq.choose (fun s -> enfa.transitions |> Map.tryFind (s,op))
                op, tos
                    |> Seq.collect id
                    |> Seq.map(fun s -> closure enfa s Epsilon)
                    |> Set.unionMany
                    
            let ops = states |> Seq.collect (Automata.getTransitionsFrom enfa)
                             |> Seq.map fst
                             |> Seq.distinct
                             |> Seq.filter (fun op -> op <> Epsilon)

            let reachable = ops |> Seq.map findReachables

            let nfaWithT = reachable |> Seq.fold (folder states) nfa
            let sdone = sdone |> Set.add states
            reachable |> Seq.fold (fun a (_,x) -> if sdone.Contains x then a else (epsilonRemoval_rec sdone enfa x a |> Option.get)) nfaWithT
                      |> Some

//            reachable |> ignore
//            nfa |> Some
        let init = closure enfa enfa.start Epsilon
        epsilonRemoval_rec (Set []) enfa init (Automata.emptyDFA |> Automata.setInitState init)

    let toDFA (nfa:Automata.NFA) : Automata.DFA option =
        let rec toDFA_rec (nfa:Automata.NFA) (states:Set<int>) (dfa:Automata.DFA) =
            let allTransitions =
                states |> Seq.collect (Automata.getTransitionsFrom nfa)
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

    let regexStringToDfa s =
        match Parser.parse(s) with
        | FParsec.CharParsers.Success (a,b,c) -> a |> toNFA |> Option.bind epsilonRemoval
        | _ -> None

    type Regex(s:string) =
        let dfa = regexStringToDfa s
        member x.isMatch(input:string) =
            match dfa with
            | None -> false
            | Some(n) -> Automata.isMatchDFA n input
        static member isMatch (regex:string, input:string) =
            match regexStringToDfa regex with
            | None -> false
            | Some n -> Automata.isMatchDFA n input
