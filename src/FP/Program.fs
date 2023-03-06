open System
open FP
open DSL

let job1 () =
    let graph = 
        state {
            let! a = Entry "a"
            let! b = Entry "b"
            let! c = Entry "c"
            let! A = Unary (fun x -> Math.Pow(x, 2)) b
            let! B = Unary (fun x -> x * 4.0) a
            let! C = Binary (*) B c
            let! D = Binary (-) A C
            let! L = Unary Math.Sqrt D
            let! M = Unary (fun x -> -x) b
            let! E = Binary (+) M L
            let! F = Binary (-) M L
            let! G = Unary (fun x -> x * 2.0) a
            let! H = Binary (/) E G
            let! K = Binary (/) F G
            do! Exit "x1" H
            do! Exit "x2" K
        }
    let g = getGraph graph
    SDF.submitValue 3 "a" g
    SDF.submitValue -12 "b" g
    SDF.submitValue 4 "c" g
    
    let x1 = SDF.getResult "x1" g
    let x2 = SDF.getResult "x2" g
    
    printfn "%A, %A" x1 x2

    
    
job1()