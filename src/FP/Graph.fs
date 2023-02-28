namespace FP

module Graph =

    type VertexData<'V> = int (* identifier *) * 'V (* vertex data *)

    type EdgeData<'E> = int (* identifier *) * int (* vertex target *) * 'E (* edge data *)

    type Adjacency<'E> = EdgeData<'E> list
    
    type Vertex<'V, 'E> = VertexData<'V> * Adjacency<'E>
    
    type Graph<'V, 'E> =
        int (* nextNode identifier *) *
        Vertex<'V, 'E> list
    
    let empty: Graph<_,_> = (0, [])
    
    let vertexId (v:Vertex<_,_>) = v |> fst |> fst
    
    let vertexData (v:Vertex<_,_>) = v |> fst |> snd
    
    let edgeId ((x,_,_):EdgeData<_>) = x
    
    let edgeTarget ((_,x,_):EdgeData<_>) = x
    
    let edgeData ((_,_,x):EdgeData<_>) = x
    
    let getVertex v (g:Graph<_, _>) : Vertex<_,_> =
        snd g |> List.find (fun V -> vertexId V = v)
        
    let getEdges v (g:Graph<_, _>) =
        g |> getVertex v |> snd
        
    let addVertex (v:'V) (g:Graph<'V, _>)
        : (int*Graph<'V,_>) =
        let id = fst g
        let s = snd g
        let newVD : VertexData<_> = (id, v)
        let newA : Adjacency<_> = []
        let newV = (newVD, newA)
        (id, (id + 1, newV::s))
        
    let addEdge (v:int) (v':int) (e:'E) (g:Graph<'V, 'E>)
        : (int*Graph<'V,'E>) =
            let id = fst g
            let s = snd g
            let newE : EdgeData<_> = (id, v', e)
            (id,
                (id + 1,
                    s |> List.map (fun V ->
                    if (vertexId V) = v then
                        (fst V, newE::(snd V))
                    else V)))
            
    let removeEdge (id:int) (g:Graph<_,_>)
        : Graph<_,_> =
        let next = fst g
        let s = snd g
        (next, s |> List.map ( fun (v, a) ->
            (v, a |> 
                List.filter (fun x -> (edgeId x) <> id))))
        
    let removeVertex (id:int) (g:Graph<_,_>) 
        : Graph<_,_> =
        let next = fst g
        let s = snd g
        (next, s |> ([] |> List.fold (fun s' (v, a) ->
            if (fst v) = id then s'
            else
            let f = fun x -> ((edgeTarget x) <> id)
            let newA = a |> List.filter f
            let newV = (v, newA)
            newV::s')))