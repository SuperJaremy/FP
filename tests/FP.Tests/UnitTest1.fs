module FP.Tests

open NUnit.Framework

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () =
    Assert.AreEqual(6UL, Fifteen.Recursion.countPaths 2)

[<Test>]
let Test2 () =
    Assert.AreEqual(4, Sixteen.Recursion.solution 2)

[<Test>]
let Test3 () =
    Assert.AreEqual(7, Sixteen.Recursion.solution 10)

[<Test>]
let Test4 () =
    Assert.AreEqual(4, Sixteen.TailRecursion.solution 2)

[<Test>]
let Test5 () =
    Assert.AreEqual(7, Sixteen.TailRecursion.solution 10)

[<Test>]
let Test6 () =
    Assert.AreEqual(6UL, Fifteen.Loop.countPaths 2)

[<Test>]
let Test7 () =
    Assert.AreEqual(4u, Sixteen.StepByStep.solution 2)

[<Test>]
let Test8 () =
    Assert.AreEqual(26u, Sixteen.StepByStep.solution 15)

[<Test>]
let Test9 () =
    Assert.AreEqual(4u, Sixteen.Loop.solution 2)

[<Test>]
let Test10 () =
    Assert.AreEqual(26u, Sixteen.Loop.solution 15)

[<Test>]
let Test11 () =
    Assert.AreEqual(6UL, Fifteen.Sequence.countPaths 2)
