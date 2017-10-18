module Blackjack 

open Cards
open System

type Model = {
    Deck : Deck 
    Hand : Hand
}

type Command = 
| Hit 
| Stand 

let update model command = 
    match command with 
    | Hit -> let (top, remaining) = model.Deck |> draw 
             match top with 
             | None -> model 
             | Some c -> {Deck = remaining; Hand = List.append model.Hand [c]}
    | Stand -> model 

let checkIfWon model = 
    let score, b = getValueOfHand model.Hand
    let mutable altScore = 0 
    match b with 
    | None -> ()
    | Some s -> altScore <- s 
    score = 21 || altScore = 21

let checkIfLost model =
    let score, b = getValueOfHand model.Hand 
    let mutable altScore = 0
    match b with 
    | None -> score > 21 
    | Some s -> 
            altScore <- s 
            score > 21 && altScore > 21

let init = 
    let model = {Deck = createStandardDeck |> shuffle ; Hand = []} 
    model
    |> update <| Hit 
    |> update <| Hit

let printStatus model = 
    printfn "Deck:"
    printCards model.Deck 
    printfn "Hand:"
    printCards model.Hand 
    let low, high = getValueOfHand model.Hand
    match high with 
    | None -> printfn "Hand Value: %i" low
    | Some h -> printfn "Hand Value: %i or %i" low h
    if checkIfLost model then 
       printfn "This is a losing hand, sorry!"
    elif checkIfWon model then 
        printfn "Blackjack! You Win!"
    model 

let rec gameloop model = 
        if not (checkIfLost model) && not (checkIfWon model) then 
            printfn "'hit' or 'stand'?"
            let input = Console.ReadLine() 
            try 
                if input = "hit" then  
                    update model Hit |> printStatus |> gameloop 
                elif input = "stand" then 
                    update model Stand |> printStatus |> gameloop
                else 
                    printfn "Did not unserstand command: '%s'" input
                    model |> gameloop 
            with 
            | ex -> printfn "%s" ex.Message





















