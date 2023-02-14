# Лабораторная работа 3
Группа: P34102  
Выполнил: Сабуров В.А.

# Общие требования
- программа должна быть реализована в функциональном стиле;
- ввод/вывод должен быть отделён от алгоритмов аппроксимации;
- алгоритм аппроксимации должен порождать замыкания, работающие на некоторых интервалах;
- требуется использовать идиоматичный для технологии стиль программирования.

# Имплементация
## Функции
```F#
type Functions =
    | Linear
    | Logarithmic
    | Exponential
    | Power
    | Segments

```
## Ввод/Вывод
### Ввод
```F#
module Input

open System
open Approximations

let parseFloat s =
    try
        Some(float s)
    with _ ->
        None

let readTwoNumbersFromStdin () =
    let line = Console.ReadLine()
    let values = line.Split '\t'

    if values.Length = 2 then
        let (x, y) = (parseFloat values[0], parseFloat values[1])

        match x, y with
        | Some a, Some b -> (a, b)
        | _ -> FormatException("Wrong input format") |> raise
    else
        FormatException("Wrong input format") |> raise

let readNewDot (outbox: ApproximationFunctions.Approximation) =
    let dot = readTwoNumbersFromStdin ()
    outbox.approximate dot

```
### Вывод
```F#
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

```
## Основной цикл
```F#
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

```
## Аппроксимация
```F#
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

    let segments dots =
        let pairs = List.pairwise dots

        let segs =
            List.map
                (fun ((a, b), (c, d)) ->
                    let k = (d - b) / (c - a)
                    let b = b - k * a
                    fun x -> k * x + b)
                pairs

        let approximations =
            Seq.map2
                (fun a b ->
                    let dot, _ = a
                    let x, _ = dot
                    x, b)
                pairs
                segs

        fun x ->
            (let _, func =
                approximations
                |> Seq.filter (fun elem ->
                    let dot, _ = elem
                    dot <= x)
                |> Seq.maxBy (fun elem ->
                    let dot, _ = elem
                    dot)

             func x)

    let private getApproximation func =
        match func with
        | Linear -> Linear, linear
        | Logarithmic -> Logarithmic, logarithmic
        | Exponential -> Exponential, exponential
        | Power -> Power, power
        | Segments -> Segments, segments

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
```