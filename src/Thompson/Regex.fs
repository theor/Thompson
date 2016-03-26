namespace Thompson

module Regex =

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

