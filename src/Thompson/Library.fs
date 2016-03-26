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
    | Union of Op * Op
    | Concat of Op * Op
    | Kleene of Op
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

type Automata = NFA.NFA

module Automatas =
    let parse (s:string) : RegEx =
        Val Null

    let opandToNFA (o:Opand) : Automata option =
        match o with
        | Char _ | Epsilon -> NFA.empty |> NFA.addTransition 0 o 1 |> Some
//        | Var v -> None
        | _ -> None

    let toNFA (r:RegEx) : Automata option =
        let rec toNFA_rec (n:int) (r:RegEx) : Automata option =
            match r with
            | Val v -> NFA.empty |> NFA.addTransition n v (n+1) |> NFA.addEndState (n+1) |> Some
            | Union(a, b) ->
                let na = toNFA_rec (n+1) a
                let naEnd = (na.Value.ends.Head)
                let nbStart = (naEnd + 1)
                let nb = toNFA_rec nbStart b
                let nbEnd = (nb.Value.ends.Head)

                NFA.empty
                |> NFA.addSubNfa na.Value
                |> NFA.addSubNfa nb.Value
                |> NFA.addTransition n Epsilon (n+1)
                |> NFA.addTransition n Epsilon (nbStart)
                |> NFA.addTransition naEnd Epsilon (nbEnd + 1)
                |> NFA.addTransition nbEnd Epsilon (nbEnd + 1)
                |> NFA.addEndState (nbEnd + 1)
                |> Some
            | Concat(a, b) ->
                let na = toNFA_rec (n) a
                let nbStart = (na.Value.ends.Head)
                let nb = toNFA_rec nbStart b
                NFA.empty
                |> NFA.addSubNfa na.Value
                |> NFA.addSubNfa nb.Value
                |> NFA.addEndState (nb.Value.ends.Head)
                |> Some
            (* n -> (nfaStart -> nfaEnd) -> nfaEnd + 1
               n -> nfaEnd + 1
               nfaEnd -> nfaStart
            *)
            | Kleene o -> 
                let nfa = toNFA_rec (n+1) o
                let nEnd = nfa.Value.ends.Head
                NFA.empty
                |> NFA.addSubNfa nfa.Value
                |> NFA.addTransition n Epsilon (n+1)
                |> NFA.addTransition nEnd Epsilon (nEnd+1)
                |> NFA.addTransition n Epsilon (nEnd+1)
                |> NFA.addTransition nEnd Epsilon (n+1)
                |> NFA.addEndState (nEnd+1)
                |> Some
            | _ -> None
        toNFA_rec 0 r
        //If the operand is a character c, then our FA has two states, s0 (the start state) and sF (the final, accepting state), and a transition from s0 to sF with label c.
        //If the operand is epsilon, then our FA has two states, s0 (the start state) and sF (the final, accepting state), and an epsilon transition from s0 to sF.
        //If the operand is null, then our FA has two states, s0 (the start state) and sF (the final, accepting state), and no transitions.
        
    let toDFA (nfa:Automata) : Automata option =
        None

module Parser = 
    open FParsec
    let parse s =
        let cha = anyOf ['a';'b';'c'] |>> (Char>>Val)
        let elementaryRE = cha
        let star = elementaryRE .>>? pchar '*' |>> Kleene
//        let plus = elementaryRE .>>? pchar '+' |>> Kleene
        let basicRE = star (*<|> plus*) <|> elementaryRE
        let simpleRE = (* concatenation <|>  *) basicRE
//            many1 letter |>> (fun x -> List.tail x |> List.fold (fun s c -> Concat(s, c |> Char |> Val)) (List.head x |> Char |> Val))
        let union, unionImpl = createParserForwardedToRef()
        // = re <|> simpleRE
        let re = union <|> simpleRE
        do unionImpl := simpleRE .>>? pchar '|' .>>.? re |>> Union
//        let union = pchar '|'
        FParsec.CharParsers.run re s