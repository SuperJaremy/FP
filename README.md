# Лабораторная работа 2


Группа: P34102

Выполнил: 
Сабуров В.А.

# Требования:

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства монойда).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования.

# Имплементация
## Структура
```F#
type AVLTree<'a> =
        | E
        | T of AVLTree<'a> * 'a * AVLTree<'a>
```
## Балансировка
```F#
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
```
### Добавление
```F#
 let rec insert a tree =
        match tree with
        | E -> T(E, a, E)
        | T (left, item, right) ->
            if a < item then
                T((insert a left), item, right) |> balanceTree
            else
                T(left, item, (insert a right)) |> balanceTree
```
### Удаление
```F#
let rec private findMax tree =
        match tree with
        | E -> InvalidOperationException("Searching for max in empty tree") |> raise
        | T (E, item, E) -> item
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
```
### Конкатенация
```F#
let rec concat tree1 tree2 =
        match tree2 with
        | E -> tree1
        | _ ->
            match tree1 with
            | E -> tree2
            | T (left, item, right) -> insert item tree2 |> concat left |> concat right
```
### Фильтрация
```F#
 let rec filter cond tree =
        match tree with
        | E -> E
        | T (left, item, right) ->
            if cond item then
                T((filter cond left), item, (filter cond right))
            else
                concat (filter cond left) (filter cond right)
```
### Отображение
```F#
    let rec map f tree =
        match tree with
        | E -> E
        | T (left, item, right) -> T((map f left), f item, (map f right))
```
### Свёртка
#### Левая
```F#
    let rec foldLeft folder state tree =
        match tree with
        | E -> state
        | T (left, item, right) ->
            foldLeft folder state left
            |> fun x -> folder x item |> fun x -> foldLeft folder x right
```
#### Правая
```F#
    let rec foldRight folder state tree =
        match tree with
        | E -> state
        | T (left, item, right) ->
            foldRight folder state right
            |> fun x -> folder x item |> fun x -> foldRight folder x left
```
## Unit-тестирование
```F#
module FP.Tests

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
let TestDeleteFromEmpty () =
    let tree = E :> AVLTree<int>
    Assert.AreEqual(tree, delete 100 E)

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
    let tree = T(T(T(E, 4, E), 1, T(E, 13, E)), 20, T(T(E, 18, E), 9, T(E, 7, E)))
    let cond = fun x -> x > 10
    Assert.AreEqual(T(T(E, 13, E), 20, T(E, 18, E)), (tree |> filter cond))

[<Test>]
let TestMap () =
    let tree = T(T(T(E, 4, E), 1, T(E, 13, E)), 20, T(T(E, 18, E), 9, T(E, 7, E)))
    let f = (*) 2

    let treeAfterMap =
        T(T(T(E, 8, E), 2, T(E, 26, E)), 40, T(T(E, 36, E), 18, T(E, 14, E)))

    Assert.AreEqual(treeAfterMap, (tree |> map f))

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
        T(T(T(E, 1, E), 2, E), 3, T(T(T(E, 3, E), 4, E), 5, T(E, 6, E)))

    Assert.AreEqual(treeAfterConcat, concat tree1 tree2)

```
