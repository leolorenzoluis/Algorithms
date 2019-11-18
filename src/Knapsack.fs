open System
open System.Collections.Generic

let mutable cacheHits = 0
let mutable cacheMisses = 0

let memoize f =
    let cache = Dictionary<_, _>()
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

type Item = { Value : int; Size  : int }  

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

let N, K = (10, 67)
printfn "N = %d, K = %d" N K

let items = genItems N

let rec combinations_cont =
    memoize (
     fun key ->
       cont {
                match key with
                | (0, _) | (_, 0)                   -> return 0
                | (i, k) when items.[i-1].Size > k  -> return! combinations_cont (i - 1, k) 
                | (i, k)                            -> let item = items.[i-1]
                                                       let! v1 = combinations_cont (i-1, k)
                                                       let! beforeItem = combinations_cont (i-1, k - item.Size)
                                                       let v2 = beforeItem + item.Value
                                                       return max v1 v2
        }
    )

let res = combinations_cont (N, K) id
printfn "Answer: %d" res
printfn "Memo hits: %d" cacheHits
printfn "Memo misses: %d" cacheMisses
printfn ""

let rec combinations_plain =
    memoize (
     fun key ->
                match key with
                | (i, k) when i = 0 || k = 0        -> 0
                | (i, k) when items.[i-1].Size > k  -> combinations_plain (i-1, k) 
                | (i, k)                            -> let item = items.[i-1]
                                                       let v1 = combinations_plain (i-1, k)
                                                       let beforeItem = combinations_plain (i-1, k-item.Size)
                                                       let v2 = beforeItem + item.Value
                                                       max v1 v2
    )

cacheHits <- 0
cacheMisses <- 0

let res2 = combinations_plain (N, K)
printfn "Answer: %d" res2
printfn "Memo hits: %d" cacheHits
printfn "Memo misses: %d" cacheMisses