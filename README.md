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
Посчитать количество путей в точку можно, взяв все точки, из которых мы можем прийти в заданную, и сложив количество путей в них. В точки на левой и верхней сторонах можно прийти только одним способом.
```F# let sideDots = matrixSide + 1 
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
## Выводы
Так как количество путей в точку зависит от количества путей в соседнюю точку, подсчёт надо производить линейно друг за другом. Рекурсивное решение оказалось крайне долгим, так как значения для одних и тех же точек подсчитываются по многу раз. Генерация последовательности позволяет ускорить этот процесс, но надо постоянно пересоздавать массив с количеством путей.
# Задача 16
## Условие
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
What is the sum of the digits of the number 2^1000?  
[link](https://projecteuler.net/problem=16)
## Решения
После подсчёта степени, которое можно произвести методом стандартной библиотеки, нам требуется разбить полученное число на цифры.

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
## Выводы
В данном случае очень удобно разбивать число на цифры генерацией последовательности.
