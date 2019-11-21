open Algorithms

[<EntryPoint>]
let main _ =
    printfn "Running algorithms"
    
    let items: Knapsack.Item[] = 
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
    let result = Knapsack.execute items 15
    printfn "Result: %A" result
    0
