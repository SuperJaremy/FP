module Output

open Approximations

type PrinterAgent() =
    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec messageLoop () =
                async {
                    let! msg = inbox.Receive()
                    let func, values = msg

                    match func with
                    | Linear -> printfn "%s" "Linear:"
                    | Logarithmic -> printfn "%s" "Logarithmic:"
                    | Exponential -> printfn "%s" "Exponential:"
                    | Power -> printfn "%s" "Power:"
                    | Segments -> printfn "%s" "Segments:"

                    for (x, y) in values do
                        printfn "x: %f y: %f" x y

                    return! messageLoop ()
                }

            messageLoop ())

    member this.print x = agent.Post x
