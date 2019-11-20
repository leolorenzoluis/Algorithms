﻿open System
open System.Collections.Generic

let mutable cacheHits = 0
let mutable cacheMisses = 0

type Item = { Value : int; Size  : int } 
let cache = Dictionary<_, _>()
let memoize f =
    fun x ->
        match cache.TryGetValue(x) with
        | (true, v) -> 
            cacheHits <- cacheHits + 1
            printfn "Hit for %A - Result is %A" x v
            v
        | _ ->
            cacheMisses <- cacheMisses + 1
            printfn "Miss for %A" x
            let res = f x
            cache.[x] <- res
            res
let items = 
    [|
        {
            Value = 20
            Size = 1
        }
        {
            Value = 5
            Size = 2
        }
        {
            Value = 10
            Size = 3
        }
        {
            Value = 40
            Size = 8
        }
        {
            Value = 15
            Size = 7
        }
        {
            Value = 25
            Size = 4
        }        
    |]

// let items = 
//     [|
//         {
//             Value = 1
//             Size = 1
//         }
//         {
//             Value = 4
//             Size = 3
//         }
//         {
//             Value = 5
//             Size = 4
//         }
//         {
//             Value = 7
//             Size = 5
//         }   
//     |]


// let items = 
//     [|
//         {
//             Value = 60
//             Size = 10
//         }
//         {
//             Value = 100
//             Size = 20
//         }
//         {
//             Value = 120
//             Size = 30
//         }
//     |]

let N, K = ((Seq.length items), 7)
let basket = List<Item>()
let mutable recursiveIndex = 1
printfn "N = %d, K = %d" N K
printfn "====================="
let rec knapsack =
    memoize (
     fun key ->
        match key with
        | (n, k) when n = 0 || k = 0        -> 
            0
        | (n, k) when items.[n-1].Size > k  -> 
            knapsack (n-1, k) 
        | (n, k)                            -> 
            // printfn "============"
            // printfn "N = %A K = %A" n k
            let item = items.[n-1]
            
            printfn ""
            printfn "============================================= %A" n
            printfn "[%A]: Current item: (%A, %A). Weight: %A" n item.Value item.Size k
            
            let excludeItem = knapsack (n-1, k)
            printfn "[%A]: LEFT  - Include item [%A] CURRENT WEIGHT  [%A]" (n-1) excludeItem k
            let includeItem = knapsack (n-1, k-item.Size)
            
            printfn "[%A]: RIGHT - Include item [%A] CURRENT WEIGHT  [%A]" (n-1) includeItem k
            printfn "============================================= %A" n
            printfn ""

            let v2 = includeItem + item.Value
            printfn "[%A]: Return value from [%A] (%A) + Current item value (%A) = %A" n (n-1) includeItem item.Value v2
            printfn "[%A]: [%A]: Left:(%A) , [%A]: Right:(%A). Weight:(%A)" n (n-1) excludeItem (n-1)  v2 k
            printfn "[%A]: Max(%A, %A)" n excludeItem v2
            max excludeItem v2
    )


let rec getItemsIncluded n weight knapsackValue (basket: ResizeArray<Item>) =
    let itemAboveMe = n - 1
    let maybeCellAboveMe = cache.TryGetValue((itemAboveMe,weight))
    match maybeCellAboveMe with
    | (true, knapsackValueFromCellAboveMe) ->
        
        if knapsackValue = knapsackValueFromCellAboveMe then
            getItemsIncluded (itemAboveMe) (weight) knapsackValue basket   
        else
            let item = items.[itemAboveMe]
            basket.Add(item)
            // Subtract the current value from the table deduct it with the knapsack value
            let remainingKnapsackValue = knapsackValue - item.Value
            let remainingWeight = weight - item.Size
            getItemsIncluded n remainingWeight remainingKnapsackValue basket
    | (false, _) -> 
            // Base case no more items
            printfn "No more cell above me %A" (itemAboveMe)  


let res = knapsack (N, K)
getItemsIncluded N K res basket

printfn "Answer: %d" res
printfn "Memo hits: %d" cacheHits
printfn "Memo misses: %d" cacheMisses
printfn "Cache: %A" cache


printfn "Basket length: %A" (Seq.length basket)
printfn "Basket: %A" (basket)