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
            test.Add(x)
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
let mutable recursiveIndex = 1
printfn "N = %d, K = %d" N K
printfn "====================="
let rec knapsack =
    memoize (
     fun key ->
        let n, k = key
        // if n < items.Length then
        // printfn "(%A, %A)" n k
        let mutable test = 0
        let mutable blah = ""
        while test <> recursiveIndex do
            blah <- blah + "=" 
            test <- test + 1
        // printfn "%A" (blah + recursiveIndex.ToString())
        // printfn "%A" (blah + (sprintf "(%A, %A)" n k))
        recursiveIndex <- recursiveIndex + 1
        match key with
        | (n, k) when n = 0 || k = 0        -> 
            printfn "Returning 0 because N - %A K - %A" n k
            0
        | (n, k) when items.[n-1].Size > k  -> 
        
            let item = items.[n-1]
                
            // printfn "Current item: (%A, %A). Weight: %A" item.Value item.Size k
            // The new item is more than the weight limit                
            // printfn "New item is more than the weight limit. %A; Weight = %A" items.[n-1] k
            knapsack (n-1, k) 
        | (n, k)                            -> 
            // printfn "============"
            // printfn "N = %A K = %A" n k
            let item = items.[n-1]
            
            printfn ""
            printfn "============================================= %A" n
            printfn "[%A]: Current item: (%A, %A). Weight: %A" n item.Value item.Size k
            // printfn "Current item value : %A size : %A" item.Value item.Size
            // printfn "Recursing v1 [%A]... n will be %A. Current weight: %A" n (n-1) k
            
            let excludeItem = knapsack (n-1, k)
            printfn "[%A]: LEFT  - Include item [%A] CURRENT WEIGHT  [%A]" (n-1) excludeItem k
            // printfn "Current item size - %A" item.Size
            // printfn "V1 - %A" v1
            // printfn ""
            // printfn "Recursing before item [%A]...n will be %A. Current weight: %A" n (n-1) k
            let includeItem = knapsack (n-1, k-item.Size)
            
            printfn "[%A]: RIGHT - Include item [%A] CURRENT WEIGHT  [%A]" (n-1) includeItem k
            printfn "============================================= %A" n
            printfn ""
            // printfn "%A" (blah + recursiveIndex.ToString())
            // printfn "%A" (blah + (sprintf "(%A, %A)" n k))
            // printfn "%A" blah
            // printfn "%A" blah
            // printfn "Item before value: %A" includeItem
            let v2 = includeItem + item.Value
            printfn "[%A]: Return value from [%A] (%A) + Current item value (%A) = %A" n (n-1) includeItem item.Value v2
            // printfn "Before item - %A" beforeItem47909245

            // printfn "V2 - %A" v2
            // printfn "============"
            // printfn "N - %A K - %A V1 - %A V2 - %A" n k excludeItem v2
            // if excludeItem > v2 then
            //     test.Add(item)
            printfn "[%A]: [%A]: Left:(%A) , [%A]: Right:(%A). Weight:(%A)" n (n-1) excludeItem (n-1)  v2 k
            printfn "[%A]: Max(%A, %A)" n excludeItem v2
            // printfn "%A" (blah + (sprintf "(%A, %A)" n k))
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

cache
|> Seq.iter(fun (key) ->
    printfn "KEY: %A. VALUE: %A" key.Key key.Value
)