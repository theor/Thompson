// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Suave

[<EntryPoint>]
let main argv = 

    startWebServer defaultConfig (Successful.OK "Hello World!")
    printfn "%A" argv
    0 // return an integer exit code
