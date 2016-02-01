// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


// Imperative version
let hello() = printfn "Hello World!"


// Version equivalent to the functional scala example 
let Write s = printfn s

type IOProgram = Write of string

let pureHello () =
  Write("Hello World")

let run (program:IOProgram) =
    match program with
    | Write msg -> printfn "%A" msg

let runPureHello() =  pureHello() |> run

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
