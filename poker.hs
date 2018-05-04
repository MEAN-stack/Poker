import Data.Function
import Data.List

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Bounded, Enum)

instance Show Value where
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

instance Read Value where
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



type Card' = (Value, Suit)

newtype Card = C Card' deriving (Eq)

instance Show Card where
    show (C (r, s)) = show r ++ show s

instance Read Card where
    readsPrec _ (xs) =  let readValue x = reads x :: [(Value, String)]
                            readSuit x = reads x :: [(Suit, String)] in
                                case readValue xs of
                                    [] -> []
                                    (r,ys):_ -> case readSuit ys of
                                       [] -> []
                                       (s,zs):_ -> [(C (r,s), zs)]


instance Ord Card where
    compare (C x) (C y) = compare (fst x) (fst y)

type Hand = [Card]

-- get the rank of a hand
-- highCard < onePair < twoPairs < threeOfAKind < straight < flush < fullHouse < fourOfAKind < straightFlush < royalFlush

data Rank = HighCard
            | OnePair
            | TwoPairs 
            | ThreeOfAKind 
            | Straight 
            | Flush 
            | FullHouse 
            | FourOfAKind 
            | StraightFlush 
            | RoyalFlush deriving (Eq, Ord)

rank :: Hand -> Rank
rank h
    | isRoyalFlush h = RoyalFlush
    | isStraightFlush h = StraightFlush
    | isFourOfAKind h = FourOfAKind
    | isFullHouse h = FullHouse
    | isFlush h = Flush
    | isStraight h = Straight
    | isThreeOfAKind h = ThreeOfAKind
    | isTwoPairs h = TwoPairs
    | isOnePair h = OnePair
    | isHighCard h = HighCard
    | otherwise = HighCard

isFlush :: Hand -> Bool
isFlush h = 1 == (length $ nub $ map getSuit h)

isFourOfAKind :: Hand -> Bool
isFourOfAKind h = (2 == length xs) && ((1 == length x) || (4 == length x))
                  where xs = group $ sort $ map getValue h
                        x = head xs

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind h = (2 == length xs) && ((2 == length x) || (3 == length x))
                  where xs = group $ sort $ map getValue h
                        x = head xs

isTwoPairs :: Hand -> Bool
isTwoPairs h = (3 == length xs) && ((1 == length x) || (2 == length x))
                  where xs = group $ sort $ map getValue h
                        x = head xs
                        
isOnePair :: Hand -> Bool
isOnePair h = (4 == length xs) && ((1 == length x) || (2 == length x))
               where xs = group $ sort $ map getValue h
                     x = head xs
                                                
isHighCard :: Hand -> Bool
isHighCard h = (5 == length xs)
               where xs = group $ sort $ map getValue h

isStraight :: Hand -> Bool
isStraight h = (isHighCard h) && (4 == ((fromEnum $ last xs) - (fromEnum $ head xs))) 
               where xs = sort $ map getValue h

isStraightFlush :: Hand -> Bool
isStraightFlush h = (isStraight h) && (isFlush h) 

isRoyalFlush :: Hand -> Bool
isRoyalFlush h = (isStraight h) && (isFlush h) && (Ten == head xs)
               where xs = sort $ map getValue h
                              
-- "8C TS KC 9H 4S 7D 2S 5D 3S AC"
readHands :: String -> (Hand, Hand)
readHands s = (map read $ take 5 ws, map read $ drop 5 ws)
               where ws = words s

compareHands :: String -> Bool
compareHands s = let (h1, h2) = readHands s in
                     h1 > h2 -- TODO: Implement the rules to compare poker hands

getSuit :: Card -> Suit
getSuit (C (_ , s)) = s

getValue :: Card -> Value
getValue (C (v , _)) = v

main = do
    fileContents <- readFile "poker.txt"
    let games = lines fileContents
    print $ length $ filter compareHands games
