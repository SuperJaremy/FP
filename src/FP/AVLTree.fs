namespace FP

open System

module AVLTree =
    type AVLTree<'a> =
        | E
        | T of AVLTree<'a> * 'a * AVLTree<'a>


    let rec private treeHeight tree =
        match tree with
        | E -> 0
        | T (left, _, right) -> (Math.Max((treeHeight left), (treeHeight right))) + 1

    let private balanceFactor tree =
        match tree with
        | T (left, _, right) -> (treeHeight right) - (treeHeight left)
        | _ -> 0

    let private rotateLeft tree =
        match tree with
        | E -> E
        | T (left, item, right) ->
            match right with
            | E -> InvalidOperationException("Invalid left rotation") |> raise
            | T (l, i, r) -> T(T(left, item, l), i, r)

    let private rotateRight tree =
        match tree with
        | E -> E
        | T (left, item, right) ->
            match left with
            | E -> InvalidOperationException("Invalid right rotation") |> raise
            | T (l, i, r) -> T(l, i, T(r, item, right))

    let private balanceTree tree =
        match tree with
        | T (left, item, right) ->
            if balanceFactor tree = 2 then
                rotateLeft (T(left, item, (if balanceFactor right < 0 then rotateRight right else right)))
            else if balanceFactor tree = -2 then
                rotateRight (T((if balanceFactor left > 0 then rotateLeft left else left), item, right))
            else
                tree
        | _ -> tree


    let rec insert a tree =
        match tree with
        | E -> T(E, a, E)
        | T (left, item, right) ->
            if a < item then
                T((insert a left), item, right) |> balanceTree
            else
                T(left, item, (insert a right)) |> balanceTree

    let rec private findMax tree =
        match tree with
        | E -> InvalidOperationException("Searching for max in empty tree") |> raise
        | T (_, item, E) -> item
        | T (_, _, right) -> findMax right

    let rec delete a tree =
        match tree with
        | E -> E
        | T (E, item, E) -> if a = item then E else tree
        | T (E, item, right) -> if a = item then right else T(E, item, (delete a right))
        | T (left, item, E) -> if a = item then left else T((delete a left), a, E)
        | T (left, item, right) ->
            if a < item then
                T((delete a left), item, right) |> balanceTree
            else if a > item then
                T(left, item, (delete a right)) |> balanceTree
            else
                let max = findMax left
                T((delete max left), max, right) |> balanceTree

    let rec private _concat tree1 tree2 state =
        match tree1, tree2 with
        | T _, T _ ->
            let max1 = findMax tree1
            let max2 = findMax tree2

            if max1 >= max2 then
                _concat (delete max1 tree1) tree2 (insert max1 state)
            else
                _concat tree1 (delete max2 tree2) (insert max2 state)
        | E, T _ ->
            let treeMax = findMax tree2
            _concat tree1 (delete treeMax tree2) (insert treeMax state)
        | T _, E ->
            let treeMax = findMax tree1
            _concat (delete treeMax tree1) tree2 (insert treeMax state)
        | _ -> state



    let concat tree1 tree2 =
        match tree2 with
        | E -> tree1
        | _ ->
            match tree1 with
            | E -> tree2
            | T _ -> _concat tree1 tree2 E

    let rec filter cond tree =
        match tree with
        | E -> E
        | T (left, item, right) ->
            if cond item then
                T((filter cond left), item, (filter cond right))
            else
                concat (filter cond left) (filter cond right)

    let rec map f tree =
        match tree with
        | E -> E
        | T (left, item, right) -> T((map f left), f item, (map f right))

    let rec foldLeft folder state tree =
        match tree with
        | E -> state
        | T (left, item, right) ->
            foldLeft folder state left
            |> fun x -> folder x item |> fun x -> foldLeft folder x right

    let rec foldRight folder state tree =
        match tree with
        | E -> state
        | T (left, item, right) ->
            foldRight folder state right
            |> fun x -> folder x item |> fun x -> foldRight folder x left

    let rec printTree tree =
        foldLeft
            (fun x y ->
                printf "%A " y
                0)
            0
            tree
        |> ignore

        printfn ""

    let tree = E
    printTree tree
    printfn ""
    let tree1 = insert 10 tree |> insert 10 |> insert 10
    printTree tree1
    printfn ""
    let tree2 = delete 10 tree1
    printTree tree2
    printfn ""

    let tree3 =
        insert 500 tree
        |> insert 400
        |> insert 600
        |> insert 300
        |> insert 450
        |> insert 700
        |> insert 650
        |> insert 800
        |> delete 400
        |> delete 450

    printTree tree3
    printfn ""

    let tree4 =
        insert 500 E
        |> insert 400
        |> insert 600
        |> insert 550
        |> insert 700
        |> insert 525
        |> insert 800

    printTree tree4
    printfn ""
    let tree5 = concat tree3 tree4
    printTree tree5
    printfn ""
    printTree (filter (fun x -> (x % 100) / 10 = 5) tree5)
    printfn ""
    printTree (map ((*) 2) tree5)
    printfn ""

    printfn
        "%d"
        (foldRight
            (fun x y ->
                printfn "%A" y
                0)
            0
            tree4)
