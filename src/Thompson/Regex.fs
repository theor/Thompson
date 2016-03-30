namespace Thompson

module Regex =
    let metaChars = ['+'; '*'; '('; ')'; '.'; '?']
    type Opand =
        | Char of char
        | Var of string
        | Any
        | Epsilon
        | Null
        static member format (op:Opand) =
            match op with
            | Char c -> if List.exists (fun x -> x = c) metaChars then "\\" + c.ToString() else c.ToString()
            | Any -> "."
            | Epsilon -> "\u03b5"
            | Null -> "\u2205"
            | Var s -> "\\" + s
    type Op =
        | Union of Op * Op
        | Concat of Op * Op
        | Kleene of Op
        | Val of Opand
        override x.ToString() = sprintf "%A" x
        static member format (op:Op) =
            match op with
            | Union(a,b) -> sprintf "%s|%s" (Op.format a) (Op.format b)
            | Concat(a,b) -> sprintf "%s%s" (Op.format a) (Op.format b)
            | Kleene(a) -> sprintf "%s*" (Op.format a)
            | Val(a) -> Opand.format a
    type RegExAst = Op

