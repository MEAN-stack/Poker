import Data.Function
import Data.List

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Bounded, Enum)

instance Show Rank where
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "T"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"

instance Read Rank where
    readsPrec _ (x:xs) = case x of
                             '2' -> [(Two, xs)]
                             '3' -> [(Three, xs)]
                             '4' -> [(Four, xs)]
                             '5' -> [(Five, xs)]
                             '6' -> [(Six, xs)]
                             '7' -> [(Seven, xs)]
                             '8' -> [(Eight, xs)]
                             '9' -> [(Nine, xs)]
                             'T' -> [(Ten, xs)]
                             'J' -> [(Jack, xs)]
                             'Q' -> [(Queen, xs)]
                             'K' -> [(King, xs)]
                             'A' -> [(Ace, xs)]
                             _   -> []


data Suit = Spades | Clubs | Hearts | Diamonds deriving (Eq, Ord, Bounded, Enum)

instance Show Suit where
    show Spades   = "S"
    show Clubs    = "C"
    show Hearts   = "H"
    show Diamonds = "D"

instance Read Suit where
    readsPrec _ (x:xs) = case x of
                             'S' -> [(Spades, xs)]
                             'C' -> [(Clubs, xs)]
                             'H' -> [(Hearts, xs)]
                             'D' -> [(Diamonds, xs)]
                             _   -> []



type Card' = (Rank, Suit)

newtype Card = C Card' deriving (Eq)

instance Show Card where
    show (C (r, s)) = show r ++ show s

instance Read Card where
    readsPrec _ (xs) =  let readRank x = reads x :: [(Rank, String)]
                            readSuit x = reads x :: [(Suit, String)] in
                                case readRank xs of
                                    [] -> []
                                    (r,ys):_ -> case readSuit ys of
                                       [] -> []
                                       (s,zs):_ -> [(C (r,s), zs)]


instance Ord Card where
    compare (C x) (C y) = compare (fst x) (fst y)

type Hand = [Card]

rank :: Hand -> Int
rank _ = 0

-- "8C TS KC 9H 4S 7D 2S 5D 3S AC"
readHands :: String -> (Hand, Hand)
readHands s = (map read $ take 5 ws, map read $ drop 5 ws)
              where ws = words s

compareHands :: String -> Bool
compareHands s = let (h1, h2) = readHands s in
                     h1 > h2


main = do
    fileContents <- readFile "poker.txt"
    let games = lines fileContents
    print $ length $ filter compareHands games
