namespace FP.Fifteen
module Recursion =
    let rec private countPaths_in (point : bigint, matrixSide : bigint) =
        if point = bigint.Zero then
            bigint.One
        elif (point < matrixSide + bigint.One) then
            countPaths_in ((point - bigint.One), matrixSide)
        elif (point % (matrixSide + bigint.One) = bigint.Zero) then
            countPaths_in ((point - matrixSide - bigint.One), matrixSide)
        else
            (countPaths_in ((point - bigint.One, matrixSide))
            + (countPaths_in ((point - (matrixSide + bigint.One)), matrixSide)))

    let countPaths matrixSide:bigint = countPaths_in (((matrixSide + bigint.One) * (matrixSide + bigint.One) - bigint.One), matrixSide)
    
    // printfn "The answer is %A" (countPaths 15I)
