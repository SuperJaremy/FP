module FP.Tests

open Microsoft.FSharp.Collections
open NUnit.Framework
open AVLTree

[<Test>]
let TestInsertToEmpty () =
    Assert.AreEqual(T(E, 5, E), (insert 5 E))

[<Test>]
let TestInsert () =
    Assert.AreEqual(T(T(E, 3, E), 5, E), T(E, 5, E) |> insert 3)

[<Test>]
let TestLeftBalancing () =
    Assert.AreEqual(T(T(E, 10, E), 10, T(E, 10, E)), insert 10 E |> insert 10 |> insert 10)

[<Test>]
let TestRightBalancing () =
    Assert.AreEqual(T(T(E, 1, E), 2, T(E, 3, E)), insert 3 E |> insert 2 |> insert 1)

[<Test>]
let TestDeleteFromEmpty () = Assert.IsTrue(eq E (delete 100 E))

[<Test>]
let TestDeleteValueNotPresentInTree () =
    let tree = T(T(E, 1, E), 2, T(E, 3, E))
    Assert.AreEqual(tree, delete 100 tree)

[<Test>]
let TestDelete () =
    let tree = T(T(E, 1, E), 2, T(E, 3, E))
    Assert.AreEqual(T(E, 2, T(E, 3, E)), delete 1 tree)

[<Test>]
let TestAVLLeftRotation () =
    let tree = T(T(E, 40, E), 50, T(T(E, 55, E), 60, T(E, 70, E)))

    let treeAfterRotation =
        T(T(T(E, 40, E), 50, T(E, 52, E)), 55, T(E, 60, T(E, 70, E)))

    Assert.AreEqual(treeAfterRotation, insert 52 tree)

[<Test>]
let TestAVLRightRotation () =
    let tree = T(T(T(E, 30, E), 40, T(E, 45, E)), 50, T(E, 60, E))

    let treeAfterRotation =
        T(T(T(E, 30, E), 40, E), 45, T(T(E, 47, E), 50, T(E, 60, E)))

    Assert.AreEqual(treeAfterRotation, insert 47 tree)

[<Test>]
let TestFilter () =
    let tree = T(T(T(E, 1, E), 4, T(T(E, 7, E), 9, E)), 13, T(E, 18, T(E, 20, E)))
    let cond = fun x -> x > 10
    Assert.AreEqual(T(T(E, 13, E), 18, T(E, 20, E)), (tree |> filter cond))

[<Test>]
let TestMap () =
    let tree = T(T(T(E, 1, E), 4, T(T(E, 7, E), 9, E)), 13, T(E, 18, T(E, 20, E)))
    let f = (*) 2

    let treeAfterMap =
        T(T(T(E, 2, E), 8, T(E, 14, E)), 18, T(T(E, 26, E), 36, T(E, 40, E)))

    Assert.AreEqual(treeAfterMap, (tree |> map f))

[<Test>]
let TestMapReordering () =
    let tree = T(T(T(E, -18, E), -9, T(E, -4, E)), 1, T(T(E, 7, E), 13, T(E, 20, E)))
    let f = abs

    let treeAfterMap =
        T(T(T(E, 1, E), 4, T(E, 7, E)), 9, T(T(E, 13, E), 18, T(E, 20, E)))

    Assert.AreEqual(treeAfterMap, tree |> map f)

[<Test>]
let TestLeftFold () =
    let tree = T(T(E, "1", E), "2", T(E, "3", E))
    let f state curr = state + curr
    Assert.AreEqual("123", (tree |> foldLeft f ""))

[<Test>]
let TestRightFold () =
    let tree = T(T(E, "1", E), "2", T(E, "3", E))
    let f state curr = state + curr
    Assert.AreEqual("321", (tree |> foldRight f ""))

[<Test>]
let TestConcat () =
    let tree1 = T(T(E, 1, E), 2, T(E, 3, E))
    let tree2 = T(T(T(E, 3, E), 4, E), 5, T(E, 6, E))

    let treeAfterConcat =
        T(T(T(E, 1, T(E, 2, E)), 3, T(E, 3, E)), 4, T(E, 5, T(E, 6, E)))

    Assert.AreEqual(treeAfterConcat, concat tree1 tree2)

[<FsCheck.NUnit.Property>]
let ``Left fold and right fold are the same if folder is commutative`` (tree: AVLTree<int>) =
    let folder sum x = sum + x
    let result1 = foldLeft folder 0 tree
    let result2 = foldRight folder 0 tree
    result1 = result2

let rec treeHeight tree =
    match tree with
    | E -> 0
    | T (left, _, right) -> (max (treeHeight left) (treeHeight right)) + 1

[<FsCheck.NUnit.Property>]
let ``Tree is balanced after inserts`` (xs: list<int>) =
    let folder state x = insert x state
    let tree = List.fold folder E xs

    match tree with
    | E -> true
    | T (left, _, right) -> abs ((treeHeight left) - (treeHeight right)) <= 1

[<FsCheck.NUnit.Property>]
let ``Tree is balanced after delete`` (xs: list<int>) =
    if xs.IsEmpty then
        true
    else
        let folder state x = insert x state
        let tree = List.fold folder E xs |> delete (List.item (xs.Length / 2) xs)

        match tree with
        | E -> true
        | T (left, _, right) -> abs ((treeHeight left) - (treeHeight right)) <= 1

[<FsCheck.NUnit.Property>]
let ``Concat is associative`` (xs1: list<string>, xs2: list<string>, xs3: list<string>) =
    let folder state x = insert x state
    let tree1 = List.fold folder E xs1
    let tree2 = List.fold folder E xs2
    let tree3 = List.fold folder E xs3
    let result1 = concat (concat tree1 tree2) tree3
    let result2 = concat tree1 (concat tree2 tree3)
    eq result1 result2

[<FsCheck.NUnit.Property>]
let ``E is identity element`` (tree: AVLTree<int>) =
    tree = concat tree E && tree = concat E tree

[<FsCheck.NUnit.Property>]
let ``Tree after insert is not eq to Tree`` (xs: list<int>, x: int) =
    let tree = List.fold (fun state x -> insert x state) E xs
    let treeAfter = insert x tree
    not (eq tree treeAfter)

[<FsCheck.NUnit.Property>]
let ``Tree equals itself`` (xs: list<int>, x: int) =
    let tree = List.fold (fun state x -> insert x state) E xs
    eq tree tree

[<FsCheck.NUnit.Property>]
let ``Filter and double filter are same`` (xs: list<int>) =
    let tree = List.fold (fun state x -> insert x state) E xs
    let cond x = x % 2 = 0
    let treeAfter = filter cond tree
    eq treeAfter (filter cond treeAfter)

[<EntryPoint>]
let main argv = 0
