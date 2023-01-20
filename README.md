 # Лабораторная работа 1






Группа: P34102

Выполнил: 
Сабуров В.А.


# Задача 15
## Условие
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?  
[link](https://projecteuler.net/problem=15)
## Решения
### Императивный язык
```C
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

uint64_t count_paths(size_t matrix_side) {
  size_t i;
  int side_dots = matrix_side + 1;
  int last_dot = side_dots * side_dots - 1;
  uint64_t ans = 0;
  uint64_t *paths = malloc(sizeof(uint64_t) * (last_dot + 1));
  for (i = 0; i < last_dot + 1; i++) {
    if (i == 0 || i < side_dots || i % side_dots == 0) {
      paths[i] = 1;
    } else {
      paths[i] = paths[i - 1] + paths[i - side_dots];
    }
  }
  ans = paths[last_dot];
  free(paths);
  return ans;
}

int main() {
  size_t matrix_side = 20;
  printf("%" PRIu64 "\n", count_paths(matrix_side));
  return 0;
}
```
### F#
Посчитать количество путей в точку можно, взяв все точки, из которых мы можем прийти в заданную, и сложив количество путей в них. В точки на левой и верхней сторонах можно прийти только одним способом.
```F# 
let sideDots = matrixSide + 1 
if point = 0 || point < sideDots || point % sideDots = 0 then
            1UL
        else
            (countPaths_in (point - 1) matrixSide
             + (countPaths_in (point - sideDots) matrixSide))
 ```
 Далее мы можем использовать этот алгоритм рекурсивно или во время генерации последовательности
 ```F#
 let seq =
            []
            |> Seq.unfold (fun state ->
                let idx = state.Length

                if idx = 0 || idx < sideDots || idx % sideDots = 0 then
                    Some(1UL, List.append state [ 1UL ])
                else
                    let paths = state[idx - 1] + state[idx - sideDots]
                    Some(paths, List.append state [ paths ]))

        Seq.item (sideDots * sideDots - 1) seq
 ```
#### Рекурсивное решение
```F#
module Recursion =
    let rec private countPaths_in point matrixSide =
        let sideDots = matrixSide + 1

        if point = 0 || point < sideDots || point % sideDots = 0 then
            1UL
        else
            (countPaths_in (point - 1) matrixSide
             + (countPaths_in (point - sideDots) matrixSide))

    let countPaths matrixSide =
        countPaths_in ((matrixSide + 1) * (matrixSide + 1) - 1) matrixSide
```
#### Хвостовая рекурсия
```F#
module TailRecursion =
    let rec private countPaths_in point matrixSide state =
        let sideDots = matrixSide + 1

        match point with
        | 0 -> [ 1UL ]
        | _ ->
            let newState = countPaths_in (point - 1) matrixSide state

            match point with
            | idx when point < sideDots || point % sideDots = 0 -> List.insertAt idx 1UL newState
            | _ -> List.insertAt point (newState[point - 1] + newState[point - sideDots]) newState

    let countPaths matrixSide =
        let point = ((matrixSide + 1) * (matrixSide + 1) - 1)
        List.item point (countPaths_in point matrixSide [])
```
#### Генерация последовательности
```F#
module Sequence =
    let countPaths matrixSide =
        let sideDots = matrixSide + 1

        let seq =
            []
            |> Seq.unfold (fun state ->
                let idx = state.Length

                if idx = 0 || idx < sideDots || idx % sideDots = 0 then
                    Some(1UL, List.append state [ 1UL ])
                else
                    let paths = state[idx - 1] + state[idx - sideDots]
                    Some(paths, List.append state [ paths ]))

        Seq.item (sideDots * sideDots - 1) seq
```
#### Цикл
```F#
module Loop =
    let countPaths matrixSide =
        let mutable list = []
        let sideDots = matrixSide + 1

        for i in 0 .. (sideDots * sideDots - 1) do
            if i = 0 || i < sideDots || i % sideDots = 0 then
                list <- List.append list [ 1UL ]
            else
                list <- List.append list [ (list[i - 1] + list[i - sideDots]) ]

        list[sideDots * sideDots - 1]
```
## Выводы
Так как количество путей в точку зависит от количества путей в соседнюю точку, подсчёт надо производить линейно друг за другом. Рекурсивное решение оказалось крайне долгим, так как значения для одних и тех же точек подсчитываются по многу раз. Генерация последовательности позволяет ускорить этот процесс, но надо постоянно пересоздавать массив с количеством путей. Решение через цикл использует изменяемое состояние, поэтому плохо вписывается в функциональную парадигму.
# Задача 16
## Условие
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
What is the sum of the digits of the number 2^1000?  
[link](https://projecteuler.net/problem=16)
## Решения
### Императивный язык
```Java
import java.math.BigInteger;

public class Sixteen {
    public static void main(String[] args) {
        int power = 1000;
        int sum = 0;
        BigInteger num = BigInteger.valueOf(2L);
        num = num.pow(power);
        while(num.compareTo(BigInteger.ZERO) > 0) {
            sum += num.remainder(BigInteger.TEN).intValue();
            num = num.divide(BigInteger.TEN);
        }
        System.out.println(sum);
    }
}
```
### F#
После подсчёта степени, которое можно произвести в том числе и методом стандартной библиотеки, нам требуется разбить полученное число на цифры.

```F#
if num < 10I then
   uint num
else
   (uint (num % 10I)) + sumOfDigits (num / 10I)
 ```
Мы можем использовать этот код рекурсивно или во время генерации последовательности. 
```F#
 let seq2 =
            num
            |> Seq.unfold (fun state ->
                if state > 0I then
                    Some(state % 10I |> uint, state / 10I)
                else
                    None)

        Seq.sum seq2
```
#### Рекурсия
```F#
module Recursion =
    let rec powerOfTwo power =
        if power = 0 then 1I else 2I * powerOfTwo (power - 1)

    let rec sumOfDigits num =
        if num < 10I then
            uint num
        else
            (uint (num % 10I)) + sumOfDigits (num / 10I)

    let solution power = powerOfTwo power |> sumOfDigits
```
#### Хвостовая рекурсия
```F#
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
```
#### Генерация последовательности
```F#
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
```
#### Цикл
```F#
module Loop =
    let solution power =
        let mutable num = bigint.Pow(2I, power)
        let mutable sum = 0u

        while num > 0I do
            sum <- sum + ((num % 10I) |> uint)
            num <- num / 10I

        sum
```
## Выводы
Решение через рекурсию и хвостовую рекурсию почти идентичны. Решение через генерацию последовательности крайне удобно, так как позволяет представить любое целое число в виде набора цифр. Решение через цикл использует изменяемое состояние, поэтому плохо вписывается в функциональную парадигму.
