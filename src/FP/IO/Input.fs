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
