namespace FP.Fifteen

module Recursion =
    let rec private countPaths_in point matrixSide =
        let sideDots = matrixSide + 1

        if point = 0 then
            1UL
        elif point < sideDots then
            countPaths_in (point - 1) matrixSide
        elif point % sideDots = 0 then
            countPaths_in (point - sideDots) matrixSide
        else
            (countPaths_in (point - 1) matrixSide
             + (countPaths_in (point - sideDots) matrixSide))

    let countPaths matrixSide =
        countPaths_in ((matrixSide + 1) * (matrixSide + 1) - 1) matrixSide

    printfn "The answer is %u" (countPaths 18)
