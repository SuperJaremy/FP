namespace Approximations

    module ApproximationSquares =
        let linear dots =
            let approx = (+) 1.0
            approx

        let logarithmic dots =
            let approx = log
            approx
    
        let exponential dots =
            let approx = exp
            approx
        
        let power dots =
            let approx = fun (x: float) -> pown x 2
            approx
    
        let polynomial dots =
            let approx = fun (x: float) -> (pown x 2) + x
            approx
        
        let approximate func =
            match func with
            | Linear -> Linear, linear
            | Logarithmic -> Logarithmic, logarithmic
            | Exponential -> Exponential, exponential
            | Power -> Power, power
            | Polynomial -> Polynomial, polynomial