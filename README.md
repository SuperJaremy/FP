# Лабораторная работа 4

Группа: P34102

Выполнил:
Сабуров В.А.

# Задание
Synchronous Data flow с расчетом и эспоротом в Dot нотацию

# Реализация
В ходе реализации было разработано три основных модуля один на базе другого:
1. Модуль для построения графов
1. Модуль для построения и расчёта Syncronous Data Flow на основе графа
1. Модуль eDSL для построения и отрисовки Syncronous Data Flow графа
## Граф
Граф представлен в виде списка пар узел - рёбра. Такая реализация позволяет нам
гибко описывать топологию графа.
```F#
namespace FP

module Graph =

    type VertexData<'V> = int (* identifier *) * 'V (* vertex data *)

    type EdgeData<'E> = int (* identifier *) * int (* vertex target *) * 'E (* edge data *)

    type Adjacency<'E> = EdgeData<'E> list

    type Vertex<'V, 'E> = VertexData<'V> * Adjacency<'E>

    type Graph<'V, 'E> = int (* nextNode identifier *) * Vertex<'V, 'E> list

    let empty: Graph<_, _> = (0, [])

    let currentNextId (g: Graph<_, _>) = fst g

    let vertexId (v: Vertex<_, _>) = v |> fst |> fst

    let vertexData (v: Vertex<_, _>) = v |> fst |> snd

    let edgeId ((x, _, _): EdgeData<_>) = x

    let edgeTarget ((_, x, _): EdgeData<_>) = x

    let edgeData ((_, _, x): EdgeData<_>) = x

    let getVertex v (g: Graph<_, _>) : Vertex<_, _> =
        snd g |> List.find (fun V -> vertexId V = v)

    let getEdges v (g: Graph<_, _>) = g |> getVertex v |> snd

    let addVertex (v: 'V) (g: Graph<'V, _>) : (int * Graph<'V, _>) =
        let id = fst g
        let s = snd g
        let newVD: VertexData<_> = (id, v)
        let newA: Adjacency<_> = []
        let newV = (newVD, newA)
        (id, (id + 1, newV :: s))

    let addEdge (v: int) (v': int) (e: 'E) (g: Graph<'V, 'E>) : (int * Graph<'V, 'E>) =
        let id = fst g
        let s = snd g
        let newE: EdgeData<_> = (id, v', e)

        (id,
         (id + 1,
          s
          |> List.map (fun V -> if (vertexId V) = v then (fst V, newE :: (snd V)) else V)))

    let removeEdge (id: int) (g: Graph<_, _>) : Graph<_, _> =
        let next = fst g
        let s = snd g
        (next, s |> List.map (fun (v, a) -> (v, a |> List.filter (fun x -> (edgeId x) <> id))))

    let removeVertex (id: int) (g: Graph<_, _>) : Graph<_, _> =
        let next = fst g
        let s = snd g

        (next,
         s
         |> ([]
             |> List.fold (fun s' (v, a) ->
                 if (fst v) = id then
                     s'
                 else
                     let f = fun x -> ((edgeTarget x) <> id)
                     let newA = a |> List.filter f
                     let newV = (v, newA)
                     newV :: s')))

```
## SDF
Для реализации SDF используется *actor model*, где каждому узлу и ребру графа
привязывается агент, который получает входные данные, обрабатывает их и отправляет
результат далее по цепочке. У такого подхода есть свои достоинства и недостатки. 
### Достоинства
- Асинхронная обработка данных - каждый агент работает самостоятельно в 
асинхронном режиме, не блокируя весь остальной пользовательский код.
- Единая точка входа - агент ничего не знает об отправителях данных, что позволяет
нам удобно описывать граф слева направо.
### Недостатки
- Единая точка входа - требуется продумать формат сообщений, поступающих на агент,
чтобы иметь представления о том, как обрабатывать данные. Например, в случае
бинарных операций требуется различать левый и правый операнды.
- Отсутствие очередей - требуется самостоятельно реализовать очереди в узлах с
несколькими входами, чтобы не потерять входящие данные.

Для построения графа используются два основных элемента: **Node** и **Edge**.

```F#
and SDFGraph = Graph.Graph<Node, Edge>

and SDF = SDFGraph * EntryNode list * ExitNode list
```

### Node
**Node** подразделяется на:
- *Binary* - бинарный оператор
- *Unary* - унарный оператор
- *Entry* -точка входа в граф
- *Exit* - точка выхода из графа  

Наличие только двух типов операторов - это серьёзное ограничение, однако их
достаточно, чтобы описать вычисление математического выражения.

```F#
    type UOperation = float -> float

    type Operation =
        | Unary of UOperation
        | Binary of BiOperation

    type Operand =
        | OperandA of Option<float>
        | OperandB of Option<float>

    type BinaryAgent(vertexId: int, func: BiOperation) =
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop listA listB =
                    async {
                        let! (graph1: SDFGraph, opOne), listA =
                            if List.isEmpty listA then
                                async.Bind(inbox.Receive(), (fun x -> async.Return(x, listA)))
                            else
                                async.Return(List.head listA, List.tail listA)

                        let! (graph2, opTwo), listB =
                            if List.isEmpty listB then
                                async.Bind(inbox.Receive(), (fun x -> async.Return(x, listB)))
                            else
                                async.Return(List.head listB, List.tail listB)

                        if graph1 <> graph2 then
                            Exception("Different graphs") |> raise

                        let listA, listB =
                            match opOne, opTwo with
                            | OperandA a, OperandB b
                            | OperandB b, OperandA a ->
                                let res =
                                    match a, b with
                                    | Some num1, Some num2 ->
                                        try
                                            Some(func num1 num2)
                                        with _ ->
                                            None
                                    | None, _
                                    | _, None -> None

                                let edges = Graph.getEdges vertexId graph1

                                List.map
                                    (fun edge ->
                                        let (EdgeData data) = Graph.edgeData edge
                                        data.Post(graph1, res))
                                    edges
                                |> ignore

                                listA, listB
                            | OperandA _, OperandA _ -> (listA @ [ (graph1, opOne) ] @ [ (graph1, opTwo) ]), listB
                            | OperandB _, OperandB _ -> listA, (listB @ [ (graph2, opOne) ] @ [ (graph2, opTwo) ])

                        return! loop listA listB
                    }

                loop [] [])

        member this.Post(graph: SDFGraph, operand) = agent.Post(graph, operand)

    and BinaryNode = BiData of BinaryAgent

    and UnaryAgent(vertexId: int, func: UOperation) =
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop () =
                    async {
                        let! (graph: SDFGraph), op = inbox.Receive()

                        let res =
                            match op with
                            | OperandA (Some a) ->
                                try
                                    Some(func a)
                                with _ ->
                                    None
                            | _ -> Exception("Operand b in unary operator " + vertexId.ToString()) |> raise

                        let edges = Graph.getEdges vertexId graph

                        List.map
                            (fun edge ->
                                let (EdgeData data) = Graph.edgeData edge

                                data.Post(graph, res))
                            edges
                        |> ignore

                        return! loop ()
                    }

                loop ())

        member this.Post(graph: SDFGraph, operand) = agent.Post(graph, operand)


    and UnaryNode = UData of UnaryAgent

    and EntryAgent(vertexId: int) =
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop () =
                    async {
                        let! graph, num = inbox.Receive()
                        let edges = Graph.getEdges vertexId graph

                        List.map
                            (fun edge ->
                                let (EdgeData data) = Graph.edgeData edge

                                data.Post(graph, num))
                            edges
                        |> ignore

                        return! loop ()
                    }

                loop ())

        member this.Post(graph: SDFGraph, num: float) = agent.Post(graph, Some num)

    and EntryNode = EnData of string * EntryAgent

    and ExitAgent() =

        let channel = Channel.CreateUnbounded()

        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop () =
                    async {
                        let! _, num = inbox.Receive()

                        let res =
                            match num with
                            | OperandA a -> a
                            | _ -> Exception("OperandB in exit") |> raise

                        channel.Writer.TryWrite(res) |> ignore
                        return! loop ()
                    }

                loop ())

        member this.Post(graph, operand) = agent.Post(graph, operand)

        member this.getValue() = channel.Reader.ReadAsync()


    and ExitNode = ExData of string * ExitAgent

    and Node =
        | Binary of int * BinaryNode
        | Unary of int * UnaryNode
        | Entry of int * EntryNode
        | Exit of int * ExitNode

        member this.Post =
            match this with
            | Binary (_, BiData a) -> a.Post
            | Unary (_, UData a) -> a.Post
            | Exit (_, ExData (_, a)) -> a.Post
            | Entry (_, EnData _) -> Exception("Try to write entry value from inside the graph") |> raise

        member this.id =
            match this with
            | Binary (a, _) -> a
            | Unary (a, _) -> a
            | Entry (a, _) -> a
            | Exit (a, _) -> a
```

### Edge
Между двумя узлами присутствует сущность **Edge**. Это такой же агент, как и 
**Node**. Он принимает на вход результат операций в **Node** и преобразует его
в формат опернда. Таким образом сам **Node** ничего не должен знать о следующем
после него узле.

```F#
and EdgeAgent(targetVertexId: int, isOperandA: bool) =
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop () =
                    async {
                        let! (graph: SDFGraph), num = inbox.Receive()
                        let vertex = Graph.getVertex targetVertexId graph
                        let operator = Graph.vertexData vertex

                        if isOperandA then
                            operator.Post(graph, OperandA num)
                        else
                            operator.Post(graph, OperandB num)

                        return! loop ()
                    }

                loop ())

        member this.Post(graph: SDFGraph, num: Option<float>) = agent.Post(graph, num)
        member this.isOperandA = isOperandA

and Edge = EdgeData of EdgeAgent
```

### Библиотека
Для построения и взаимодействия с графом реализован ряд методов:
- Добавление узла
```F#
let addEntryPoint (id: string) (graph: SDF) =
        let g, entries, exits = graph
        let vertexId = Graph.currentNextId g
        let entryNode = EnData(id, EntryAgent(vertexId))
        let node = Entry(vertexId, entryNode)
        let _, g = Graph.addVertex node g
        node, SDF(SDFGraph(g), entries @ [ entryNode ], exits)

    let addUnaryOperator (operation: UOperation) (previousNode: Node) (graph: SDF) =
        let g, entries, exits = graph
        let vertexId = Graph.currentNextId g
        let unaryNode = UData(UnaryAgent(vertexId, operation))
        let node = Unary(vertexId, unaryNode)
        let _, newGraph = Graph.addVertex node g
        let previousId = previousNode.id
        let edge = EdgeData(EdgeAgent(vertexId, true))
        let _, newGraph = Graph.addEdge previousId vertexId edge newGraph
        node, SDF(SDFGraph(newGraph), entries, exits)

    let addBinaryOperator (operation: BiOperation) (previousNodeA: Node) (previousNodeB: Node) (graph: SDF) =
        let g, entries, exits = graph
        let vertexId = Graph.currentNextId g
        let binaryNode = BiData(BinaryAgent(vertexId, operation))
        let node = Binary(vertexId, binaryNode)
        let _, newGraph = Graph.addVertex node g
        let previousIdA, previousIdB = previousNodeA.id, previousNodeB.id
        let edgeA = EdgeData(EdgeAgent(vertexId, true))
        let edgeB = EdgeData(EdgeAgent(vertexId, false))
        let _, newGraph = Graph.addEdge previousIdA vertexId edgeA newGraph
        let _, newGraph = Graph.addEdge previousIdB vertexId edgeB newGraph
        node, SDF(SDFGraph(newGraph), entries, exits)

    let addExit (id: string) (previousNode: Node) (graph: SDF) =
        let g, entries, exits = graph
        let vertexId = Graph.currentNextId g
        let exitNode = ExData(id, ExitAgent())
        let node = Exit(vertexId, exitNode)
        let edge = EdgeData(EdgeAgent(vertexId, true))
        let _, newGraph = Graph.addVertex node g
        let _, newGraph = Graph.addEdge previousNode.id vertexId edge newGraph
        node, SDF(SDFGraph(newGraph), entries, exits @ [ exitNode ])
```
- Отправка значения в граф для дальнейших вычислений
```F#
let submitValue (value: float) (entryId: string) (graph: SDF) =
        let g, entries, _ = graph

        let entry =
            List.find
                (fun entryNode ->
                    match entryNode with
                    | EnData (id, _) -> id = entryId)
                entries

        match entry with
        | EnData (_, agent) -> agent.Post((g, value))
```

- Получения результата вычислений
```F#
    let getResult (exitId: string) (graph: SDF) =
        let _, _, exits = graph

        let exit =
            List.find
                (fun exitNode ->
                    match exitNode with
                    | ExData (id, _) -> id = exitId)
                exits

        match exit with
        | ExData (_, agent) ->
            let tsk = agent.getValue ()
            Async.AwaitTask(tsk.AsTask()) |> Async.RunSynchronously
```

Все входы и выходы имеют текстовый идентификатор, чтобы пользователю было удобно
выбирать конкретное ребро для отправки или получения.

Данная библиотека ограничивает множество топологий,
которое пользователь может описать. Так, с помощбю этих методов не получится
создать цикл или подать значения с несколькиз узлов на один вход другого узла.

## eDSL
При помощи технологии *computation expression* языка *F#* и монады *State* мы
можем реализовать eDSL-like методы создания узлов.
### State
Обобщённая реализация монады *State* на языке *F#*
```F#
module State =
    type S<'State, 'Value> = S of ('State -> 'Value * 'State)

    let runS (S f) state = f state

    let returnS x =
        let run state = x, state
        S run

    let bindS f xS =
        let run state =
            let x, newState = runS xS state
            runS (f x) newState

        S run

    type StateBuilder() =
        member this.Return(x) = returnS x
        member this.ReturnFrom(xS) = xS
        member this.Bind(xS, f) = bindS f xS

    let state = StateBuilder()

    let getS =
        let run state = state, state
        S run

    let putS newState =
        let run _ = (), newState
        S run

```

### eDSL
Реализация eDSL-like методов для создания и отрисовки графа на основе *State*
```F#
module DSL =
    type DrawableSDF = SDF.SDF * (int * string) list

    let graph = State.state

    let submitValue value id (dsdf: DrawableSDF) =
        let sdf, _ = dsdf
        SDF.submitValue value id sdf

    let getResult id (dsdf: DrawableSDF) =
        let sdf, _ = dsdf
        SDF.getResult id sdf

    let draw (dsdf: DrawableSDF) =
        let sdf, notes = dsdf
        let graph, _, _ = sdf
        let _, vertexes = graph
        let finder id (vertexId, note) = id = vertexId
        let findNote id = List.find (finder id) notes |> snd
        let nodeDrawer id label = id + " [label=\"" + label + "\"];\n"

        let drawNode id =
            nodeDrawer (id.ToString()) (findNote id)

        let edgeDrawer id1 id2 label =
            id1 + "->" + id2 + " [label=\"" + label + "\"];\n"

        let edgeListDraw (vertexId: int) (lst: Graph.EdgeData<SDF.Edge> list) =
            List.fold
                (fun state x ->
                    let _, targetId, data = x
                    let (SDF.EdgeData agent) = data

                    state
                    + (edgeDrawer (vertexId.ToString()) (targetId.ToString()) (if agent.isOperandA then "a" else "b")))
                ""
                lst

        let drawer (drawn: string) (toDraw: Graph.Vertex<SDF.Node, SDF.Edge>) =
            let vd, adj = toDraw
            let id, _ = vd
            let drawn = drawn + (drawNode id)
            let edgeList = adj
            drawn + (edgeListDraw id edgeList)

        (List.fold drawer "strict digraph G{\n" vertexes) + "}"

    let empty = SDF.empty, []

    let getGraph sdf = State.runS sdf empty |> snd

    let Entry (id: string) =
        graph {
            let! sdf, notes = State.getS
            let node, newSdf = SDF.addEntryPoint id sdf
            do! State.putS (newSdf, notes @ [ node.id, id ])
            return node
        }

    let Unary (op: SDF.UOperation) (previous: SDF.Node) (note: string) =
        graph {
            let! sdf, notes = State.getS
            let node, newSdf = SDF.addUnaryOperator op previous sdf
            do! State.putS (newSdf, (notes @ [ node.id, note ]))
            return node
        }

    let Binary (op: SDF.BiOperation) (previousA: SDF.Node) (previousB: SDF.Node) (note: string) =
        graph {
            let! sdf, notes = State.getS
            let node, newSdf = SDF.addBinaryOperator op previousA previousB sdf
            do! State.putS (newSdf, (notes @ [ node.id, note ]))
            return node
        }

    let Exit (id: string) (previous: SDF.Node) =
        graph {
            let! sdf, notes = State.getS
            let node, newSdf = SDF.addExit id previous sdf
            do! State.putS (newSdf, (notes @ [ node.id, id ]))
            return ()
        }
```

# Пример использования
Реализация алгоритма расчёта корней квадратного уравнения
```F#
let dsdf =
    graph {
        let! a = Entry "a"
        let! b = Entry "b"
        let! c = Entry "c"
        let! A = Unary (fun x -> Math.Pow(x, 2)) b "^2"
        let! B = Unary (fun x -> x * 4.0) a "*4"
        let! C = Binary (*) B c "*"
        let! D = Binary (-) A C "-"
        let! L = Unary Math.Sqrt D "sqrt"
        let! M = Unary (fun x -> -x) b "-"
        let! E = Binary (+) M L "+"
        let! F = Binary (-) M L "-"
        let! G = Unary (fun x -> x * 2.0) a "*2"
        let! H = Binary (/) E G "/"
        let! K = Binary (/) F G "/"
        do! Exit "x1" H
        do! Exit "x2" K
    }

let g = getGraph dsdf
submitValue 3 "a" g
submitValue -12 "b" g
submitValue 4 "c" g

let x1 = getResult "x1" g
let x2 = getResult "x2" g

printfn "%A, %A" x1 x2
// printfn "%s" (draw g)
```

В качестве результата вычислений мы получим значения `Option<float>`. Метод draw
вернёт строку - граф, экспортированный в Dot нотацию.  
## Вывод программы
`Some 3.632993162, Some 0.3670068381`

## Визуализация graphviz:  
![strict digraph G{
33 [label="x2"];
31 [label="x1"];
28 [label="/"];
28->33 [label="a"];
25 [label="/"];
25->31 [label="a"];
23 [label="*2"];
23->28 [label="b"];
23->25 [label="b"];
20 [label="-"];
20->28 [label="a"];
17 [label="+"];
17->25 [label="a"];
15 [label="-"];
15->20 [label="a"];
15->17 [label="a"];
13 [label="sqrt"];
13->20 [label="b"];
13->17 [label="b"];
10 [label="-"];
10->13 [label="a"];
7 [label="*"];
7->10 [label="b"];
5 [label="*4"];
5->7 [label="a"];
3 [label="^2"];
3->10 [label="a"];
2 [label="c"];
2->7 [label="b"];
1 [label="b"];
1->15 [label="a"];
1->3 [label="a"];
0 [label="a"];
0->23 [label="a"];
0->5 [label="a"];
}
](Roots.jpg)

# Выводы
Возникает множество вопросов к целесообразности принятых решений. *Actor model*
накладывает серьёзные ограничения на узлы - чтобы расширить количество входов,
требуется изменить формат сообщений и разработать агент, способный обрабатывать
эти входы. Библиотека *SDF* сильно ограничивает возможные топологии графа, но 
при этом недостаточно fool-proof - можно создать узел, не передающий данные.
*Computation expression* позволяет создать eDSL-like синтаксис, но не полноценный
eDSL.