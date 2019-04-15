module ICPC
open System
open System.ComponentModel.DataAnnotations
open System
open System
open System.Text.RegularExpressions

let containword actualWord list =
    let rec moveThroughList list =
        match list with 
        | [] -> false
        | a::b -> 
            let word,position =a
            match word = actualWord with 
            | true -> true
            | false -> moveThroughList  b
    moveThroughList list

let contains list input=
    let rec moveThroughList list outputTuple =
        match list with 
        | [] -> outputTuple
        | a::b -> 
            let word,position =a
            match word = input with 
            | true -> moveThroughList [] position
            | false -> moveThroughList b ""
    moveThroughList list ""

let stringEditor list words=
    let rec edit stopCase outPut inputList =
        match stopCase with
        | [] -> Some (outPut.ToString().TrimStart(' '))
        | a::b::c -> 
            let alpha A =
                match A.ToString().IndexOf('.')> -1 with
                | true -> A.ToString().TrimEnd('.')
                | false -> 
                    match A.ToString().IndexOf(',')> -1 with 
                    |true -> A.ToString().TrimEnd(',')
                    | false -> A.ToString()
            match (containword (alpha a) inputList) || (containword (alpha a) inputList) with
            | false ->
                match containword (alpha b) inputList with 
                | true -> edit (b::c) (outPut+" "+a.ToString()) (((alpha a),"a")::inputList)
                | false -> edit (b::c) (outPut+" "+a.ToString()) inputList
            | true -> 
                let positionOfComma =  contains inputList (alpha a) 
                match positionOfComma with
                | "a" -> 
                    match a.ToString().IndexOf('.')= -1, a.ToString().IndexOf(',')= -1 with
                    | true, true -> 
                        match containword (alpha b) inputList with
                        | false -> edit (b::c) (outPut+" "+(alpha a)+",") (((alpha b),"b")::inputList)
                        | true -> edit (b::c) (outPut+" "+(alpha a)+",") (((alpha b),"b")::inputList)
                    | _,_ ->  
                        match a.ToString() = b.ToString() with 
                        | false -> edit (b::c) (outPut+" "+a.ToString()) inputList
                        | true -> edit (b::c) (outPut+" "+a.ToString()) (((alpha b),"b")::inputList)
                | "b" -> 
                    let justCheck1 = outPut.ToString().Split(' ')
                    let justCheck2 = justCheck1 |> Array.toList
                    let num = justCheck2.Item(justCheck1.Length-1)
                    match num.IndexOf('.')= -1, num.IndexOf(',')= -1 with
                    | true, true -> edit (b::c) (outPut+", "+a.ToString()) inputList
                    | _,_ -> edit (b::c) (outPut+" "+a.ToString()) inputList
        | a::b ->
            let alphaA = a.ToString().TrimEnd('.')
            let alphaA2 = a.ToString().TrimEnd(',')
            match (containword alphaA inputList) || (containword alphaA2 inputList) with
            | false -> edit b (outPut+" "+a.ToString()) words
            | true -> 
                let positionOfComma =  contains inputList alphaA 
                match positionOfComma = "b" with
                | true -> edit b (outPut+", "+a.ToString()) words
                | false -> edit b (outPut+" "+a.ToString()) words
    edit list "" words

let commaSprinkler input =

    let startOFList = input.ToString().Split(' ')
    let finalList = startOFList |> Array.toList
    match finalList.Length with
    | 1 -> 
        match input.ToString().IndexOf(',') with
        | -1 -> 
            let stringWord = finalList.Item(0)
            match stringWord.IndexOf('.')=(stringWord.Length-1)  with 
            | true-> Some stringWord
            | false-> None 
        | _ -> None
    | 0 -> None
    | _ -> 
        let rec move list words  =
            match list with 
            | [] -> stringEditor finalList words 
            | a::b::c->                
                match a.ToString().IndexOf(',') with
                | -1 -> move (b::c) words 
                | _ ->
                    let wordsAddA = a.ToString().TrimEnd(',')
                    let wordsAddB = b.ToString().TrimEnd('.')
                    move (b::c) ((wordsAddA,"a")::(wordsAddB, "b")::words)
            | a::b -> 
                 match a.ToString().IndexOf(',') with
                 | -1 -> move b words 
                 | _ ->
                     let wordsAddA = a.ToString().TrimEnd(',')
                     move b ((wordsAddA,"a")::words)
        move finalList []

let getLength xs=
   let rec lenght list size =
       match list with
       | [] -> size
       | a::b -> lenght b (size+1)
   lenght xs 0

let rivers input =
    let lastIndex = input.ToString().Length-1
    let stringInputArray = input.ToString().Split(' ')
    let listString = stringInputArray|> Array.toList 
    let rec checkLength list num =
        match list with 
        | [] -> num
        | a::b -> 
            match a.ToString().Length > 80  with 
            | true -> checkLength b 1
            | false -> checkLength b num
    match (checkLength listString 0) with 
    | 1 -> None
    | 0 -> 
        match listString.Length < 2, input.ToString().Contains("  ") with
        |true, _ -> None
        |_,true -> None
        |_->    
            match input.ToString().IndexOf(' ') = 0, input.ToString().LastIndexOf(' ') = lastIndex with
            |true,_-> None
            |_,true -> None
            |_-> 
                match input.ToString().Contains(","), input.ToString().Contains("!") with
                |true,_ -> None
                |_,true -> None
                |_ -> 
                    let stringInputArray = input.ToString().Split(' ')
                    let listString = stringInputArray|> Array.toList 
                    match listString.Length=2 with
                    | true -> Some (input.ToString().Length,1)
                    | false ->
                        let rec longestString list longestWidth =
                            match list with 
                            | [] -> longestWidth
                            | a::b -> 
                                match (a.ToString().Length>= longestWidth) with 
                                | true -> longestString b (a.ToString().Length)
                                | false ->longestString b longestWidth
                        let startingLength = longestString listString 0
                        let rec riverSetUp list outputList length addString =
                            match list with 
                            | [] -> ( List.rev (addString::outputList))
                            | a::b ->
                                let trimed = addString.ToString().TrimStart(' ') 
                                let addFinal = trimed+" "+a.ToString()
                                let trimFinal = addFinal.TrimStart(' ')
                                match trimFinal.Length <= length with 
                                | true -> riverSetUp b outputList length trimFinal
                                | false -> 
                                    match a.ToString().Length= length with
                                    | true -> 
                                        match trimed=(a.ToString()) && a.ToString().Length= length with
                                        | true -> riverSetUp (b) (trimed::outputList) length ""
                                        | false -> riverSetUp (a::b) (trimed::outputList) length (a.ToString())
                                    | false -> riverSetUp (a::b) (trimed::outputList) length ""
                        let rec spacePostions list indexes indexesList counter= 
                            match list with 
                            | [] -> ( List.rev indexesList)
                            | a::b -> 
                                let num = a.ToString().IndexOf(' ')
                                let remaining = a.ToString().Remove(0,num+1)
                                match remaining = "" with 
                                | true -> spacePostions b [] (indexes::indexesList) 0
                                | false ->
                                    match a.ToString().IndexOf(' ') with
                                    | -1 -> spacePostions b [] ((List.rev indexes)::indexesList) 0
                                    | _ ->
                                        let num = a.ToString().IndexOf(' ')
                                        let remaining = a.ToString().Remove(0,num+1)
                                        let trimFinal = remaining.TrimStart(' ')
                                        match indexes.Length>0 with
                                        | true ->  
                                            let check = (indexes.[counter])
                                            spacePostions (trimFinal::b) ((num+check+1)::indexes) indexesList (counter)
                                        | false ->spacePostions (trimFinal::b) (num::indexes) indexesList (counter)
                        let rec countRivers numRiver position indexList final =
                            match indexList with 
                            | [] -> final
                            | a::b::c -> 
                                let rec moveThrough itemslist1 itemslist2= 
                                    match itemslist1,itemslist2 with 
                                    | [],_ -> 
                                        match numRiver>= final with
                                        | true -> countRivers 1 position (b::c) numRiver
                                        | false -> countRivers 1 position (b::c) final
                                    | a::e,[] -> moveThrough e b 
                                    | a::e,A::B -> 
                                        match a=A || a+1=A || a-1=A with
                                        | true -> 
                                            match position = A || position + 1 = A || position - 1= A with 
                                            | true -> countRivers (numRiver+1) A (b::c) final
                                            | false ->
                                                match e = [] with 
                                                | true -> countRivers (1+1) A (b::c) final
                                                | false -> moveThrough (a::e) B 
                                        | false -> moveThrough (a::e) B 
                                moveThrough a b 
                            | a::c -> 
                                let rec moveThrough part1 num pos =
                                    match part1 with 
                                    | [] -> final
                                    | item1::item2 -> 
                                        match item1=position || item1=position+1 with 
                                        | true -> moveThrough item2 (final+1) item1
                                        | false -> moveThrough item2 final position 
                                moveThrough a final position
                        let rec finisher start previousAmount length saveMaxLength saveNumberRivers=
                            let list = (riverSetUp listString  [] length "")
                            let indexes = (spacePostions list [] [] 0)
                            let currAmountRivers= (countRivers 1 0 indexes 0)
                            let checkStop= previousAmount
                            let checkLength=length
                            match input.ToString().Length > length  with
                            | true -> 
                                match checkStop < currAmountRivers && saveNumberRivers <> currAmountRivers with
                                | true -> finisher (start) currAmountRivers (length+1) checkLength currAmountRivers
                                | false -> finisher (start) checkStop (length+1) saveMaxLength saveNumberRivers
                            | false -> 
                                let list = (riverSetUp listString  [] saveMaxLength "")
                                let indexes = (spacePostions list [] [] 0)
                                let currAmountRivers= (countRivers 1 0 indexes 0)
                                Some((saveMaxLength),currAmountRivers)
                        finisher 1 1 startingLength 0 0



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
