module TestClass

open NUnit.Framework
open FsUnit
open Algorithms.Knapsack
open System.Collections.Generic

let inline assertBasket basket expectedBasket =
    Seq.zip basket expectedBasket
    |> Seq.iter(fun zip ->
        let result, expectedResult = zip
        result.Size |> should equal expectedResult.Size
        result.Value |> should equal expectedResult.Value
    )
[<Test>]
let ``When the following items that has a sizes of 10 20 30 are provided with a given weight 10 then it should return only 1 item``() = 
    
    let items = 
        [|
            {
                Value = 60
                Size = 10
            }
            {
                Value = 100
                Size = 20
            }
            {
                Value = 120
                Size = 30
            }
        |]

    let expectedBasket = List<Item>([
            {
                Value = 60
                Size = 10
            }
        ])
    
    let knapsackValue, basket = execute items 10
    knapsackValue |> should equal 60
    basket.Count |> should equal 1
    assertBasket basket expectedBasket

[<Test>]
let ``When the following items that has a sizes of 10 20 30 are provided with a given weight 30 then it should return only 1 item with value of 160``() = 
    
    let items = 
        [|
            {
                Value = 60
                Size = 10
            }
            {
                Value = 100
                Size = 20
            }
            {
                Value = 120
                Size = 30
            }
        |]
    // The result returns descending order
    let expectedBasket = List<Item>([
            {
                Value = 100
                Size = 20
            }
            {
                Value = 60
                Size = 10
            }
        ])
    
    let knapsackValue, basket = execute items 30
    knapsackValue |> should equal 160
    basket.Count |> should equal 2
    
    assertBasket basket expectedBasket

[<Test>]
let ``When the following items that has a sizes of 1 3 4 5 are provided with a given weight 7 then it should return value of 9 with 2 items with value of 5 4``() = 
    
    let items = 
        [|
            {
                Value = 1
                Size = 1
            }
            {
                Value = 4
                Size = 3
            }
            {
                Value = 5
                Size = 4
            }
            {
                Value = 7
                Size = 5
            }   
        |]
    // The result returns descending order
    let expectedBasket = List<Item>([
            {
                Value = 5
                Size = 4
            }
            {
                Value = 4
                Size = 3
            }
        ])
    
    let knapsackValue, basket = execute items 7
    knapsackValue |> should equal 9
    basket.Count |> should equal 2
    
    assertBasket basket expectedBasket

[<Test>]
let ``When the following items that has a sizes of 10 20 30 are provided with a given weight 50 then it should return value of 220 with 2 items with value of 30 20``() = 
    
    let items = 
        [|
            {
                Value = 60
                Size = 10
            }
            {
                Value = 100
                Size = 20
            }
            {
                Value = 120
                Size = 30
            }
        |]
    // The result returns descending order
    let expectedBasket = List<Item>([
            {
                Value = 120
                Size = 30
            }
            {
                Value = 100
                Size = 20
            }
        ])
    
    let knapsackValue, basket = execute items 50
    knapsackValue |> should equal 220
    basket.Count |> should equal 2
    
    assertBasket basket expectedBasket
