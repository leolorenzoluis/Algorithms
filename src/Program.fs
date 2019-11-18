// Learn more about F# at http://fsharp.org

open System

type Job = {
    StartTime: int;
    FinishTime: int;
    Value: int;
}

let jobComparator a b =
    a.FinishTime < b.FinishTime

let latestNonConflict (jobs: Job list) =
    jobs
    |> Seq.tryFind(fun x -> x.FinishTime <= (jobs |> Seq.last).StartTime)

let findMaxProfitRec jobs acc =
    let rec loop i j = function
        | 0 -> i
        | n -> loop j (i+j) (n-1)
    loop jobs acc
findMaxProfitRec 5

let jobs = [
    { 
        StartTime = 3
        FinishTime = 10
        Value = 20
    }
    { 
        StartTime = 1
        FinishTime = 2
        Value = 50
    }
    { 
        StartTime = 6
        FinishTime = 19
        Value = 100
    }
    { 
        StartTime = 2
        FinishTime = 100
        Value = 200
    }
]


let findMaxProfit jobs n =
    let sortedByFinishTime = jobs |> Seq.sortByDescending (fun job -> job.FinishTime)

    findMaxProfitRec sortedByFinishTime n

[<EntryPoint>]
let main argv =
    let n = jobs |> Seq.length
    printfn "The optimal profit is %A" findMaxProfit jobs n
    0 // return an integer exit code
