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

module Sequence =
    let countPaths matrixSide =
        let sideDots = matrixSide + 1

        let seq =
            []
            |> Seq.unfold (fun state ->
                let idx = state.Length

                if idx = 0 || idx < sideDots || idx % sideDots = 0 then
                    Some(1UL, List.append state [ 1UL ])
                else
                    let paths = state[idx - 1] + state[idx - sideDots]
                    Some(paths, List.append state [ paths ]))

        Seq.item (sideDots * sideDots - 1) seq

module Loop =
    let countPaths matrixSide =
        let mutable list = []
        let sideDots = matrixSide + 1

        for i in 0 .. (sideDots * sideDots - 1) do
            if i = 0 || i < sideDots || i % sideDots = 0 then
                list <- List.append list [ 1UL ]
            else
                list <- List.append list [ (list[i - 1] + list[i - sideDots]) ]

        list[sideDots * sideDots - 1]
