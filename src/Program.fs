open System
open System.Collections.Generic

let mutable cacheHits = 0
let mutable cacheMisses = 0

type Item = { Value : int; Size  : int } 
let test = List() 
let cache = Dictionary<_, _>()
let memoize f =
    fun x ->
        match cache.TryGetValue(x) with
        | (true, v) -> 
            cacheHits <- cacheHits + 1
            printfn "Hit for %A - Result is %A" x v
            // test.Add(x)
            v
        | _ ->
            cacheMisses <- cacheMisses + 1
            printfn "Miss for %A" x
            let res = f x
            cache.[x] <- res
            res


type ContinuationBuilder() = 
    member b.Bind(x, f) = fun k -> x (fun x -> f x k)
    member b.Return x = fun k ->  k x
    member b.ReturnFrom x = x

let cont = ContinuationBuilder()

let genItems n = 
   [| for i = 1 to n do
         let size = i % 5
         let value = (size * i)
         yield { Value = value; Size = size }
   |]


// let items = genItems N
// let items = 
//     [|
//         {
//             Value = 20
//             Size = 1
//         }
//         {
//             Value = 5
//             Size = 2
//         }
//         {
//             Value = 10
//             Size = 3
//         }
//         {
//             Value = 40
//             Size = 8
//         }
//         {
//             Value = 15
//             Size = 7
//         }
//         {
//             Value = 25
//             Size = 4
//         }        
//     |]

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


// let items = 
//     [|
//         {
//             Value = 1
//             Size = 2
//         }
//         {
//             Value = 2
//             Size = 3
//         }
//         {
//             Value = 5
//             Size = 3
//         }
//         {
//             Value = 9
//             Size = 4
//         }   
//     |]

let N, K = ((Seq.length items), 7)
let basket = List<Item>()
printfn "N = %d, K = %d" N K
printfn "====================="
let rec knapsack =
    memoize (
     fun key ->
        let n, k = key
        printfn "N: %A Remaining weight: %A" n k
        match key with
        | (n, k) when n = 0 || k = 0        -> 
            // printfn "Returning 0 because N - %A K - %A" n k
            0
        | (n, k) when items.[n-1].Size > k  -> 
            // The new item is more than the weight limit                
            printfn "New item is more than the weight limit. %A; K = %A" items.[n-1] k
            knapsack (n-1, k) 
        | (n, k)                            -> 
            // printfn "============"
            // printfn "N = %A K = %A" n k
            let item = items.[n-1]
            printfn "Current item value : %A size : %A" item.Value item.Size
            printfn ""
            printfn "Recursing v1 [%A]... n will be %A. Current weight: %A" n (n-1) k
            let excludeItem = knapsack (n-1, k)
            // printfn "Current item size - %A" item.Size
            // printfn "V1 - %A" v1
            printfn ""
            printfn "Recursing before item [%A]...n will be %A. Current weight: %A" n (n-1) k
            let includeItem = knapsack (n-1, k-item.Size)
            printfn "Item before value: %A" includeItem
            let v2 = includeItem + item.Value
            // printfn "Before item - %A" beforeItem
            // printfn "V2 - %A" v2
            // printfn "============"
            printfn "N - %A K - %A V1 - %A V2 - %A" n k excludeItem v2
            if excludeItem > v2 then
                test.Add(item)
            max excludeItem v2
    )

cacheHits <- 0
cacheMisses <- 0

let res2 = knapsack (N, K)
printfn "Answer: %d" res2
printfn "Basket: %A" (Seq.length basket)
printfn "Memo hits: %d" cacheHits
printfn "Memo misses: %d" cacheMisses
printfn "Cache: %A" cache
printfn "Test: %A" test