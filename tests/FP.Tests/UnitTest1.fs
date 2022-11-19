module FP.Tests

open FP.Fifteen
open NUnit.Framework

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () = Assert.AreEqual(6I, Recursion.countPaths 2I)

