namespace Approximations
    
    type Methods =
    | Splines
    | Squares
    
    module Method =
        let by method =
            match method with
            | Squares -> (Squares, ApproximationSquares.approximate)
            | Splines -> (Splines, ApproximationSplines.approximate)