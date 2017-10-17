open System
open Cards 

[<EntryPoint>]
let main argv =   
    let blackjack = Blackjack.init 
    blackjack
    |> Blackjack.printStatus
    |> ignore
    0
