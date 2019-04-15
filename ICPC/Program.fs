module ICPC
open System

let commaSprinkler input =
    failwith "Not implemented"

let rivers input =
    let lastIndex = input.ToString().Length-1
    let stringInputArray = input.ToString().Split(' ')
    let listString = stringInputArray|> Array.toList 
    match input.ToString().Length>80, listString.Length < 2, input.ToString().Contains("  ") with
    |true, _,_ -> None
    |_,true, _ -> None
    |_,_,true -> None
    |_->    
        match input.ToString().IndexOf(' ') = 0, input.ToString().LastIndexOf(' ') = lastIndex with
        |true,_-> None
        |_,true -> None
        |_-> 
            match input.ToString().Contains(","), input.ToString().Contains("!") with
            |true,_ -> None
            |_true -> None
            |_ -> Some 2
            
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
