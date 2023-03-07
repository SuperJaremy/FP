namespace FP

open System.Collections.Generic
open System.Text

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
