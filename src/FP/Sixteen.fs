namespace FP.Sixteen


module Recursion =
    let rec powerOfTwo power =
        if power = 0 then 1I else 2I * powerOfTwo (power - 1)

    let rec sumOfDigits num =
        if num < 10I then
            uint num
        else
            (uint (num % 10I)) + sumOfDigits (num / 10I)

    let solution power = powerOfTwo power |> sumOfDigits

module TailRecursion =
    let rec private powerOfTwo_in power acc =
        if power = 0 then
            acc
        else
            powerOfTwo_in (power - 1) (2I * acc)

    let powerOfTwo power = powerOfTwo_in power 1I

    let rec private sumOfDigits_in (num: bigint, acc) =
        if num < 10I then
            acc + (uint num)
        else
            sumOfDigits_in (num / 10I, acc + ((num % 10I) |> uint))

    let sumOfDigits num = sumOfDigits_in (num, 0u)

    let solution power = powerOfTwo power |> sumOfDigits

module StepByStep =
    let solution power =
        let seq1 = 1I |> Seq.unfold (fun state -> Some(state, state * 2I))
        let num = Seq.item power seq1

        let seq2 =
            num
            |> Seq.unfold (fun state ->
                if state > 0I then
                    Some(state % 10I |> uint, state / 10I)
                else
                    None)

        Seq.sum seq2

module Loop =
    let solution power =
        let mutable num = bigint.Pow(2I, power)
        let mutable sum = 0u

        while num > 0I do
            sum <- sum + ((num % 10I) |> uint)
            num <- num / 10I

        sum
