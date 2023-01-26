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
