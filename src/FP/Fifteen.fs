namespace FP.Fifteen

module Recursion =
    let rec private countPaths_in point matrixSide =
        let sideDots = matrixSide + 1

        if point = 0 || point < sideDots || point % sideDots = 0 then
            1UL
        else
            (countPaths_in (point - 1) matrixSide
             + (countPaths_in (point - sideDots) matrixSide))

    let countPaths matrixSide =
        countPaths_in ((matrixSide + 1) * (matrixSide + 1) - 1) matrixSide

    // printfn "The answer is %u" (countPaths 18)

// module Cycle =
//     
//     let countPaths matrixSide =
//         let list = List.empty
//         let sideDots = matrixSide + 1
//         let lastDot = sideDots * sideDots - 1
//         for n in 0 .. (lastDot + 1) do
//             if n < sideDots || n % sideDots = 0 then
//                 [ 1UL ]::list
//             else
//                 [ list.Item(n - 1) + list.Item(n - sideDots) ]
//
//         list[lastDot]
//         
//     // printfn "The answer is %u" (countPaths 18)
    
        