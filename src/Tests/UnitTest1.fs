namespace Tests.Tests

open NUnit.Framework
open FsUnit

[<TestClass>]
type TestClass() =

    [<SetUp>]
    member this.Setup() = ()



    [<Test>]
    member this.Meh() =
        let ``When 2 is added to 2 expect 4``() = this.Add(2, 2) |> should equal 4

        let ``When ToLower(), expect lowercase letters``() = "FSHARP".ToLower() |> should startWith "fs"
        ()


    [<Test>]
    member this.Test1() = Assert.Pass()

    member this.Add(x, y) = x + y
