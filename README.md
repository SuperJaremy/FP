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

<code>let sideDots = matrixSide + 1 
 if point = 0 || point < sideDots || point % sideDots = 0 then 1UL els (countPaths_in (point - 1) matrixSide + (countPaths_in (point - sideDots) matrixSide))</code>
## Выводы
Так как количество путей в точку зависит от количества путей в соседнюю точку, подсчёт надо производить линейно друг за другом. Рекурсивное решение оказалось крайне долгим, так как значения для одних и тех же точек подсчитываются по многу раз. Генерация последовательности позволяет ускорить этот процесс, но надо постоянно пересоздавать массив с количеством путей.
# Задача 16
## Условие
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
What is the sum of the digits of the number 2^1000?  
[link](https://projecteuler.net/problem=16)
## Решения
После подсчёта степени, которое можно произвести методом стандартной библиотеки, нам требуется разбить полученное число на цифры.

``if num < 10I then
   uint num
else
   (uint (num % 10I)) + sumOfDigits (num / 10I)``
## Выводы
В данном случае очень удобно разбивать число на цифры генерацией последовательности.
