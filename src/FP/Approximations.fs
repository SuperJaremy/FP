namespace Approximations

open System
open System.Collections.Generic
open Microsoft.FSharp.Core

module ApproximationFunctions =

    let private lin dots =
        let sx, sxx, sy, sxy =
            List.fold
                (fun (sx, sxx, sy, sxy) (x, y) -> (sx + x, sxx + (x * x), sy + y, sxy + (x * y)))
                (0.0, 0.0, 0.0, 0.0)
                dots

        let n = List.length dots

        let delta, delta1, delta2 =
            (sxx * (float n) - sx * sx), (sxy * (float n) - sx * sy), (sxx * sy - sx * sxy)

        ((delta1 / delta), (delta2 / delta))

    let linear dots =
        let a, b = lin dots
        fun x -> a * x + b

    let logarithmic dots =
        let newDots = List.map (fun (x, y) -> (log x, y)) dots
        let a, b = lin newDots
        fun x -> a * (log x) + b

    let exponential dots =
        let newDots = List.map (fun (x, y) -> (x, log y)) dots
        let a, b = lin newDots
        fun x -> (exp b) * (exp (a * x))

    let power dots =
        let newDots = List.map (fun (x, y) -> (log x, log y)) dots
        let a, b = lin newDots
        fun x -> (exp b) * Math.Pow(x, a)

    let splines dots = fun x -> x

    let private getApproximation func =
        match func with
        | Linear -> Linear, linear
        | Logarithmic -> Logarithmic, logarithmic
        | Exponential -> Exponential, exponential
        | Power -> Power, power
        | Splines -> Splines, splines

    let rec private handleList (inbox: MailboxProcessor<'Msg>) list =
        async {
            let! dot = inbox.Receive()
            let newList = list @ [ dot ]

            match newList.Length with
            | len when len < 10 -> return! handleList inbox newList
            | len when len = 10 -> return newList
            | len when len > 10 -> return newList.Tail
            | _ -> return []
        }

    type Approximation(printer: Output.PrinterAgent, functions, interval) =
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec messageLoop dots =
                    async {
                        let! newDots = handleList inbox dots

                        let approximations =
                            functions
                            |> List.map (fun func ->
                                let (name, func) = getApproximation func
                                name, func newDots)

                        let start, _ = newDots.Head
                        let finish, _ = List.last newDots
                        let generated = seq { start..interval..finish }

                        let calculated =
                            approximations
                            |> List.map (fun (name, func) -> name, (generated |> Seq.map (fun x -> (x, func x))))

                        for i in calculated do
                            printer.print i

                        return! messageLoop newDots
                    }

                messageLoop [])

        member this.approximate dot = agent.Post dot
