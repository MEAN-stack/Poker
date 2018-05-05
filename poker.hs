import Data.Function
import Data.List

--
-- Card values
--
data Value =
      Two
    | Three 
    | Four 
    | Five 
    | Six 
    | Seven 
    | Eight 
    | Nine 
    | Ten 
    | Jack 
    | Queen 
    | King 
    | Ace 
    deriving (Eq, Ord, Bounded, Enum)

--
-- We provide our own implementation of read and show
--
instance Show Value where
    show v = case v of
        Two   -> "2"
        Three -> "3"
        Four  -> "4"
        Five  -> "5"
        Six   -> "6"
        Seven -> "7"
        Eight -> "8"
        Nine  -> "9"
        Ten   -> "T"
        Jack  -> "J"
        Queen -> "Q"
        King  -> "K"
        Ace   -> "A"

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

--
-- Card suits
--
data Suit = 
      Spades 
    | Clubs 
    | Hearts 
    | Diamonds 
    deriving (Eq, Ord, Bounded, Enum)

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

--
-- a Card is a value-suit pair
-- we need to wrap it in a constructor C
-- so that we can make it an instance of Read, Show and Ord 
--
type Card' = (Value, Suit)

newtype Card = C Card' deriving (Eq)

instance Show Card where
    show (C (v, s)) = show v ++ show s

instance Read Card where
    readsPrec _ (xs) = 
        let readValue x = reads x :: [(Value, String)]
            readSuit  x = reads x :: [(Suit, String)] in
                case readValue xs of
                    []       -> []
                    (v,ys):_ -> case readSuit ys of
                        []       -> []
                        (s,zs):_ -> [(C (v,s), zs)]

--
-- sort cards based on value
--
instance Ord Card where
    compare (C a) (C b) = compare (fst a) (fst b)

--
-- a hand is a list of 5 Cards
--
type Hand = [Card]

--
-- get the rank of a hand
-- highCard < onePair < twoPairs < threeOfAKind < straight < flush < fullHouse < fourOfAKind < straightFlush < royalFlush
--
data Rank = 
      HighCard
    | OnePair
    | TwoPairs 
    | ThreeOfAKind 
    | Straight 
    | Flush 
    | FullHouse 
    | FourOfAKind 
    | StraightFlush 
    | RoyalFlush
    deriving (Eq, Ord, Show)

rank :: Hand -> Rank
rank h
    | isRoyalFlush    h = RoyalFlush
    | isStraightFlush h = StraightFlush
    | isFourOfAKind h   = FourOfAKind
    | isFullHouse h     = FullHouse
    | isFlush h         = Flush
    | isStraight h      = Straight
    | isThreeOfAKind h  = ThreeOfAKind
    | isTwoPairs h      = TwoPairs
    | isOnePair h       = OnePair
    | isHighCard h      = HighCard
    | otherwise         = HighCard

--
-- to compare hands of equal rank
-- we produce a list of card values
-- values of FourOfAKinds, ThreeOfAKinds, Pairs first, then decreasing single card values
-- 
-- e.g.
-- [K,Q,J,J,Q] -> [Q,J K]
-- [T,8,A,7,T] -> [T,A,8,7]
--
rank' :: Hand -> [Value]
rank' h =
    case rank h of
        -- high card hands are straightforward
        HighCard -> reverse $ sort $ map getValue h

        -- for other hands we sort the values, then group them,
        -- then sort the groups by length and then reverse (so FourOfAKinds come first, etc)
        -- finally remove duplicates from each group, and flatten the list
        _        -> concat $ map nub $ reverse $ sortBy (compare `on` length) $ group $ sort $ map getValue h


isFlush :: Hand -> Bool
isFlush h =
    1 == (length $ nub $ map getSuit h)

isFourOfAKind :: Hand -> Bool
isFourOfAKind h =
    (2 == length xs) && ((1 == length x) || (4 == length x))
    where
        xs = group $ sort $ map getValue h
        x = head xs

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind h = 
    (3 == length xs) && not (isTwoPairs h)
    where
        xs = group $ sort $ map getValue h

isTwoPairs :: Hand -> Bool
isTwoPairs h =
    (3 == length xs) && (x /= 3)
    where
        xs = sortBy (compare `on` length) $ group $ sort $ map getValue h
        x = length $ last xs
                        
isOnePair :: Hand -> Bool
isOnePair h =
    (4 == length xs) && ((1 == length x) || (2 == length x))
    where
        xs = group $ sort $ map getValue h
        x = head xs
                                                
isHighCard :: Hand -> Bool
isHighCard h = (5 == length xs)
               where xs = group $ sort $ map getValue h

isStraight :: Hand -> Bool
isStraight h = (isHighCard h) && (4 == ((fromEnum $ last xs) - (fromEnum $ head xs))) 
               where xs = sort $ map getValue h

isFullHouse :: Hand -> Bool
isFullHouse h = (2 == length xs) && ((2 == length x) || (3 == length x))
                  where xs = group $ sort $ map getValue h
                        x = head xs

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
compareHands s
    | (rank h1 > rank h2) = True
    | (rank h1 < rank h2) = False
    | otherwise = if (rank' h1 > rank' h2) then True else False
    where (h1, h2) = readHands s

getSuit :: Card -> Suit
getSuit (C (_ , s)) = s

getValue :: Card -> Value
getValue (C (v , _)) = v

main = do
    fileContents <- readFile "poker.txt"
    let games = lines fileContents
    print $ length $ filter compareHands games
