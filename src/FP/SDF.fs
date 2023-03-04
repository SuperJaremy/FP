namespace FP

open System
open System.Threading.Channels
open Microsoft.FSharp.Control
open Microsoft.FSharp.Core

module SDF =
    type BiOperation = float -> float -> float

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

    and Edge = EdgeData of EdgeAgent

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

    and SDFGraph = Graph.Graph<Node, Edge>

    and SDF = SDFGraph * EntryNode list * ExitNode list

    let empty = SDF(SDFGraph(Graph.empty), [], [])

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
        SDF(SDFGraph(newGraph), entries, exits @ [ exitNode ])


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
