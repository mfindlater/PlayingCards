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
| Hearts
| Spades
| Diamonds
| Clubs

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

let draw deck  =
    match deck with 
    | head::tail -> (Some head, tail)
    | [] -> (None, [])

let shuffle deck =
    let random = System.Random()
    deck |> List.sortBy(fun card -> random.Next()); 
 
let createStandardDeck = 
    [(Ace,Hearts);
     (Ace,Spades);
     (Ace,Diamonds);
     (Ace, Clubs);
     (King,Hearts);
     (King,Spades);
     (King,Diamonds);
     (King, Clubs);
     (Queen,Hearts);
     (Queen,Spades);
     (Queen,Diamonds);
     (Queen, Clubs);
     (Jack,Hearts);
     (Jack,Spades);
     (Jack,Diamonds);
     (Jack, Clubs);
     (Two,Hearts);
     (Two,Spades);
     (Two,Diamonds);
     (Two, Clubs);
     (Three,Hearts);
     (Three,Spades);
     (Three,Diamonds);
     (Three, Clubs);
     (Four,Hearts);
     (Four,Spades);
     (Four,Diamonds);
     (Four, Clubs);
     (Five,Hearts);
     (Five,Spades);
     (Five,Diamonds);
     (Five, Clubs);
     (Six,Hearts);
     (Six,Spades);
     (Six,Diamonds);
     (Six, Clubs);
     (Seven,Hearts);
     (Seven,Spades);
     (Seven,Diamonds);
     (Seven, Clubs);
     (Eight,Hearts);
     (Eight,Spades);
     (Eight,Diamonds);
     (Eight, Clubs);
     (Nine,Hearts);
     (Nine,Spades);
     (Nine,Diamonds);
     (Nine, Clubs);
     (Ten,Hearts);
     (Ten,Spades);
     (Ten,Diamonds);
     (Ten, Clubs);]

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

let getNameOfSuit suit =
    match suit with 
    | Hearts -> "Hearts"
    | Spades -> "Spades"
    | Diamonds -> "Diamonds"
    | Clubs -> "Clubs"

let getNameOfCard (rank,suit) =
     let r = getNameOfRank rank
     let s = getNameOfSuit suit 
     r + " of " + s

let printCards cards =
    cards |> List.iter (fun card ->
                            let info = getNameOfCard card 
                            printfn "%s" info)
