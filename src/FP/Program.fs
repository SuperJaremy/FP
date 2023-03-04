open System
open FP
open SDF

let job () =
    let sdf = empty

    let a, sdf = addEntryPoint "a" sdf
    let b, sdf = addEntryPoint "b" sdf
    let c, sdf = addEntryPoint "c" sdf
    let A, sdf = addUnaryOperator (fun x -> Math.Pow(x, 2)) b sdf
    let B, sdf = addUnaryOperator (fun x -> x * 4.0) a sdf
    let C, sdf = addBinaryOperator (*) B c sdf
    let D, sdf = addBinaryOperator (-) A C sdf
    let L, sdf = addUnaryOperator Math.Sqrt D sdf
    let M, sdf = addUnaryOperator (fun x -> -x) b sdf
    let E, sdf = addBinaryOperator (+) M L sdf
    let F, sdf = addBinaryOperator (-) M L sdf
    let G, sdf = addUnaryOperator (fun x -> x * 2.0) a sdf
    let H, sdf = addBinaryOperator (/) E G sdf
    let K, sdf = addBinaryOperator (/) F G sdf
    let sdf = addExit "x1" H sdf
    let sdf = addExit "x2" K sdf
    
    submitValue 3 "a" sdf

    submitValue -12 "b" sdf

    submitValue 4 "c" sdf

    let x1 = getResult "x1" sdf 
    let x2 = getResult "x2" sdf
    

    printfn "%A, %A" x1 x2
    
job()