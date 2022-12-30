module FP.Tests

open FP.Fifteen
open NUnit.Framework

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () = Assert.AreEqual(6UL, Recursion.countPaths 2)

[<Test>]
let Test2 () = Assert.AreEqual(4, Sixteen.Recursion.solution 2)

[<Test>]
let Test3 () = Assert.AreEqual(7, Sixteen.Recursion.solution 10)

[<Test>]
let Test4 () = Assert.AreEqual(4, Sixteen.TailRecursion.solution 2)

[<Test>]
let Test5 () = Assert.AreEqual(7, Sixteen.TailRecursion.solution 10)

