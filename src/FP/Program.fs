open Argu
open Microsoft.FSharp.Core

let rec operate functions dots interval =
    let newDot = Input.readNewDot ()
    let newDots = List.append dots [ newDot ]

    if List.length newDots < 7 then
        operate functions newDots interval
    else
        let (first, _) = newDots.Head
        let (last, _) = List.last newDots
        let generated = seq { first..interval..last }

        let calculated =
            List.map (fun (x, (y, z)) -> (x, y, Seq.map (fun value -> (value, value |> z newDots)) generated)) functions

        List.map Output.printApproximation calculated |> ignore
        operate functions newDots interval


type Arguments =
    | [<AltCommandLine("-m"); ExactlyOnce>] Method of Approximations.Methods list
    | [<AltCommandLine("-f"); ExactlyOnce>] Function of Approximations.Functions list
    | [<AltCommandLine("-i"); ExactlyOnce>] Interval of float

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Method _ -> "approximation method"
            | Function _ -> "type of function to approximate to"
            | Interval _ -> "interval between calculated dots"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>()
    let results = parser.Parse argv
    let methods = results.GetResults Method |> List.head
    let funcs = results.GetResults Function |> List.head
    let interval = results.GetResults Interval |> List.head
    let approxes = List.map Approximations.Method.by methods

    let functions =
        List.fold (fun state (x, y) -> List.map (fun elem -> (x, y elem)) funcs |> List.append state) [] approxes

    operate functions [] interval
    0
