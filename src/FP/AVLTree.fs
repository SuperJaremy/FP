namespace FP

open System

module AVLTree =
    type AVLTree<'a> =
        | E
        | T of AVLTree<'a> * 'a * AVLTree<'a>

        member x.Height =
            match x with
            | E -> 0
            | T (left, _, right) -> (max left.Height right.Height) + 1

    let private balanceFactor tree =
        match tree with
        | T (left, _, right) -> right.Height - left.Height
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

    let rec concat tree1 tree2 =
        match tree2 with
        | E -> tree1
        | _ ->
            match tree1 with
            | E -> tree2
            | T (left, item, right) -> insert item tree2 |> concat left |> concat right

    let rec private insertTreeByElement from into =
        match from with
        | E -> into
        | T (E, item, E) -> insert item into
        | T (left, item, right) -> insertTreeByElement left into |> insertTreeByElement right |> insert item

    let rec filter cond tree =
        match tree with
        | E -> E
        | T (E, item, E) -> if cond item then tree else E
        | T (left, item, E) ->
            if cond item then
                T(filter cond left, item, E)
            else
                filter cond left
        | T (E, item, right) ->
            if cond item then
                T(E, item, filter cond right)
            else
                filter cond right
        | T (left, item, right) ->
            // if cond item then
            //     concat (filter cond left |> insert item) (filter cond right)
            // else
            //     concat (filter cond left) (filter cond right)
            let leftTree = filter cond left
            let rightTree = filter cond right

            let newTree =
                if leftTree.Height >= rightTree.Height then
                    insertTreeByElement rightTree leftTree
                else
                    insertTreeByElement leftTree rightTree

            if cond item then insert item newTree else newTree



    let rec private _map f tree state =
        match tree with
        | E -> state
        | T (left, item, right) -> insert (f item) state |> _map f left |> _map f right

    let map f tree =
        match tree with
        | E -> E
        | T _ -> _map f tree E

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

    let toList tree1 =
        let folder state x = state @ [ x ]
        foldLeft folder [] tree1

    let eq tree1 tree2 = (toList tree1) = (toList tree2)
