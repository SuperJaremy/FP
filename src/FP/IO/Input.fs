module Input
    let mutable state = (0.0,1.0)
    let readNewDot () =
        let (x, y) = state
        state <- (x + 1.0, y)
        state

