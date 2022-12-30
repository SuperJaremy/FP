namespace FP.Sixteen

module Recursion =
    let rec powerOfTwo power =
        if power = 0 then
            1I
        else
            2I * powerOfTwo (power - 1)

    let rec sumOfDigits num =
        if num < 10I then
            num |> uint
        else
            ((num % 10I) |> uint) + sumOfDigits (num / 10I)

    let solution power = powerOfTwo power |> sumOfDigits
    
module TailRecursion =
    let rec private powerOfTwo_in power acc =
        if power = 0 then
            acc
        else
            powerOfTwo_in (power - 1) (2I * acc)
            
    let powerOfTwo power = powerOfTwo_in power 1I
    
    let rec private sumOfDigits_in (num:bigint, acc) =
        if num < 10I then
            acc + (num |> uint)
        else
            sumOfDigits_in (num / 10I, acc + ((num % 10I) |> uint))
            
    let sumOfDigits num = sumOfDigits_in (num, 0u)
    
    let solution power = powerOfTwo power |> sumOfDigits
    
module StepByStep =
    let seq1 =
        1I
        |> Seq.unfold (fun state ->
            Some(state, state * 2I))
        
    printfn "%A" ((Seq.skip 10 seq1) |> Seq.head)