module Cards 

type Rank = 
| Ace
| King
| Queen
| Jack
| Ten 
| Nine
| Eight
| Seven
| Six 
| Five 
| Four 
| Three 
| Two

type Suit = 
| Heart
| Spade
| Diamond
| Club

type Card = Rank * Suit

type Hand =  Card list

type Deck = Card list 

let getValueOfCard card = 
    match card with
    | ( Ace, _) -> 1, Some(11)
    | ( King, _) -> 10, None
    | ( Queen, _) -> 10, None
    | ( Jack, _) -> 10, None
    | ( Two, _) -> 2, None
    | ( Three, _) -> 3, None
    | ( Four, _) -> 4, None
    | ( Five, _) -> 5, None
    | ( Six, _) -> 6, None
    | ( Seven, _) -> 7, None
    | ( Eight, _) -> 8, None
    | ( Nine, _) -> 9, None 
    | ( Ten, _) -> 10,  None 

let getValueOfHand cards = 
    let mutable low = 0
    let mutable high = 0 
    cards |> List.iter (fun c -> 
                                let l, h = getValueOfCard c 
                                low <- low + l
                                match h with 
                                | Some n -> high <- high + n 
                                | None -> ())
    if high = 0 then
        low, None
    else 
        low, Some(high)

let draw (deck : Deck) =
    match deck with 
    | head::tail -> (Some head, tail)
    | [] -> (None, [])

let shuffle deck =
    let random = System.Random()
    deck |> List.sortBy(fun card -> random.Next()); 
 
let createStandardDeck = 
    [(Ace,Heart);
     (Ace,Spade);
     (Ace,Diamond);
     (Ace, Club);
     (King,Heart);
     (King,Spade);
     (King,Diamond);
     (King, Club);
     (Queen,Heart);
     (Queen,Spade);
     (Queen,Diamond);
     (Queen, Club);
     (Jack,Heart);
     (Jack,Spade);
     (Jack,Diamond);
     (Jack, Club);
     (Two,Heart);
     (Two,Spade);
     (Two,Diamond);
     (Two, Club);
     (Three,Heart);
     (Three,Spade);
     (Three,Diamond);
     (Three, Club);
     (Four,Heart);
     (Four,Spade);
     (Four,Diamond);
     (Four, Club);
     (Five,Heart);
     (Five,Spade);
     (Five,Diamond);
     (Five, Club);
     (Six,Heart);
     (Six,Spade);
     (Six,Diamond);
     (Six, Club);
     (Seven,Heart);
     (Seven,Spade);
     (Seven,Diamond);
     (Seven, Club);
     (Eight,Heart);
     (Eight,Spade);
     (Eight,Diamond);
     (Eight, Club);
     (Nine,Heart);
     (Nine,Spade);
     (Nine,Diamond);
     (Nine, Club);
     (Ten,Heart);
     (Ten,Spade);
     (Ten,Diamond);
     (Ten, Club);]




let getNameOfRank rank =
    match rank with 
    | Ace -> "Ace"
    | King -> "King"
    | Queen -> "Queen"
    | Jack -> "Jack"
    | Ten -> "Ten"
    | Nine -> "Nine"
    | Eight -> "Eight"
    | Seven -> "Seven"
    | Six -> "Six"
    | Five -> "Five"
    | Four -> "Four"
    | Three -> "Three"
    | Two -> "Two"

let getPluralNameOfSuit suit =
    match suit with 
    | Heart -> "Hearts"
    | Spade -> "Spades"
    | Diamond -> "Diamonds"
    | Club -> "Clubs"

let getNameOfCard (rank,suit) =
     let r = getNameOfRank rank
     let s = getPluralNameOfSuit suit 
     r + " of " + s

let printCards cards =
    cards |> List.iter (fun card ->
                            let info = getNameOfCard card 
                            printfn "%s" info)
