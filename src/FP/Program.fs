open Approximations
open Argu
open Microsoft.FSharp.Core

type Arguments =
    | [<AltCommandLine("-f"); ExactlyOnce>] Function of Functions list
    | [<AltCommandLine("-i"); ExactlyOnce>] Interval of float

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Function _ -> "type of function to approximate to"
            | Interval _ -> "interval between calculated dots"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>()
    let results = parser.Parse argv
    let funcs = results.GetResults Function |> List.head
    let interval = results.GetResults Interval |> List.head

    let pAgent = Output.PrinterAgent()
    let aAgent = ApproximationFunctions.Approximation(pAgent, funcs, interval)

    let rec operate () =
        Input.readNewDot aAgent
        operate ()

    operate ()

    0
