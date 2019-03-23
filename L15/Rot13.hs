-- Listing 15.2

data FourLetterAlphabet = L1 | L2 | L3 | L4
  deriving (Show, Enum, Bounded)


rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize symbol = toEnum shiftedSymbolAsInt
  where
    symbolAsInt = fromEnum symbol
    shiftedSymbolAsInt = (symbolAsInt + halfAlphabet) `mod` alphabetSize
    halfAlphabet = alphabetSize `div` 2


listEnum :: (Bounded a, Enum a) => [a]
listEnum = [minBound .. maxBound]


{-- How do I implement a function to calculate the size of any
 -- Bounded, Enum type?
 -- the following doesn't work
enumSize :: (Bounded a, Enum a) => Int
enumSize = fromEnum (maxBound :: a) + 1
--}
