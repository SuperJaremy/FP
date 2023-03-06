namespace FP

module DSL =
    type S<'State, 'Value> =
        S of ('State -> 'Value * 'State)

    let runS (S f) state = f state
    
    let returnS x =
        let run state =
            x, state
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
        let run state =
            state, state
        S run
        
    let putS newState =
        let run _ =
            (), newState
        S run
        
    let getGraph sdf =
        runS sdf SDF.empty |> snd
        
    let Entry (id: string) = state {
        let! sdf = getS
        let node, newSdf = SDF.addEntryPoint id sdf
        do! putS newSdf
        return node
    }
    
    let Unary (op: SDF.UOperation) (previous: SDF.Node) = state {
        let! sdf = getS
        let node, newSdf = SDF.addUnaryOperator op previous sdf
        do! putS newSdf
        return node
    }
    
    let Binary (op: SDF.BiOperation) (previousA: SDF.Node) (previousB: SDF.Node) = state {
        let! sdf = getS
        let node, newSdf = SDF.addBinaryOperator op previousA previousB sdf
        do! putS newSdf
        return node
    }
    
    let Exit (id: string) (previous: SDF.Node) = state {
        let! sdf = getS
        let newSdf = SDF.addExit id previous sdf
        do! putS newSdf
        return ()
    }
    
    