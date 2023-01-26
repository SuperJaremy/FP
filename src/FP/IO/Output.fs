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
        for (x,y) in values do
            printfn "x: %f y: %f" x y

