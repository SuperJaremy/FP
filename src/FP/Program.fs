open System
open FP
open DSL

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
printfn "%s" (draw g)
