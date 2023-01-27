# Лабораторная работа 3
Группа: P34102  
Выполнил: Сабуров В.А.

# Общие требования
- программа должна быть реализована в функциональном стиле;
- ввод/вывод должен быть отделён от алгоритмов аппроксимации;
- алгоритм аппроксимации должен порождать замыкания, работающие на некоторых интервалах;
- требуется использовать идиоматичный для технологии стиль программирования.

# Имплементация
## Структура
### Методы
```F#
type Methods =
    | Splines
    | Squares

module Method =
    let by method =
        match method with
        | Squares -> (Squares, ApproximationSquares.approximate)
        | Splines -> (Splines, ApproximationSplines.approximate)
```
### Функции
```F#
type Functions =
    | Linear
    | Logarithmic
    | Exponential
    | Power
```
## Ввод/Вывод
### Ввод
```F#
module Input

open System

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

let readNewDot () =
    let dot = readTwoNumbersFromStdin ()
    dot
```
### Вывод
```F#
module Output

let printApproximation (method, func, values) =
    match method with
    | Approximations.Squares -> printfn "%s" "Squares:"
    | Approximations.Splines -> printfn "%s" "Splines:"

    match func with
    | Approximations.Linear -> printfn "%s" "Linear:"
    | Approximations.Logarithmic -> printfn "%s" "Logarithmic:"
    | Approximations.Exponential -> printfn "%s" "Exponential:"
    | Approximations.Power -> printfn "%s" "Power:"

    for (x, y) in values do
        printfn "x: %f y: %f" x y
```
## Основной цикл
```F#
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
```
## Аппроксимация
### Метод наименьших квадратов
```F#
namespace Approximations

open System
open Microsoft.FSharp.Core

module ApproximationSquares =

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

    let approximate func =
        match func with
        | Linear -> Linear, linear
        | Logarithmic -> Logarithmic, logarithmic
        | Exponential -> Exponential, exponential
        | Power -> Power, power
```