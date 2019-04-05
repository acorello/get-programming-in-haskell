-- Listing 15.2

data FourLetterAlphabet = L1 | L2 | L3 | L4
  deriving (Show, Enum, Bounded)

type Bit = Bool
type Byte = [Bit]
type Bytes = [Byte]

t = True
f = False


rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize symbol = toEnum shiftedSymbolAsInt
  where
    shiftedSymbolAsInt = (symbolAsInt + halfAlphabet) `mod` alphabetSize
    symbolAsInt = fromEnum symbol
    halfAlphabet = alphabetSize `div` 2

-- TODO: review how this works, i.e. how return-type polimorphism works
listEnum :: (Bounded a, Enum a) => [a]
listEnum = [minBound .. maxBound]

{-- How do I implement a function to calculate the size of any
 -- Bounded, Enum type?
 -- the following doesn't work
-- TODO: return type polimophism seems to fail composibility in this case, or am I missing something?
enumSize :: (Bounded a, Enum a) => Int
enumSize = fromEnum (maxBound :: a) + 1
--}

-- Listing 15.8
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder alphabetSize symbol = toEnum encodedSymbol
  where encodedSymbol = offset `mod` alphabetSize
        offset        = fromEnum symbol + halfAlphabet + padding
        padding       = if even alphabetSize then 0 else 1
        halfAlphabet  = alphabetSize `div` 2


intToByte :: Int -> Byte
intToByte i = reverse $ take wordSize $ intToByte' i ++ padding
  where padding = repeat False


intToByte' :: Int -> Byte
intToByte' 0 = [False]
intToByte' 1 = [True]
intToByte' i = lsb : intToByte' (div i 2)
  where lsb | mod i 2 == 0 = False
            | otherwise    = True


wordSize = length $ intToByte' maxBound


xorBit :: Bit -> Bit -> Bit
xorBit l r = not (l == r)


xorPair :: (Bit, Bit) -> Bit
xorPair (l,r) = xorBit l r

  
xorByte :: Byte -> Byte -> Byte
xorByte ls rs = map xorPair $ zip ls rs


charToByte :: Char -> Byte
charToByte c = intToByte $ fromEnum c


byteToInt :: Byte -> Int
byteToInt byte = let reversedByte = reverse byte
                     indexedByte = zip reversedByte [0..]
                     significantPairs = filter fst indexedByte
                     nonZeroByteIndices = map snd significantPairs
                 in sum $ map (2^) nonZeroByteIndices


byteToChar :: Byte -> Char
byteToChar = toEnum . byteToInt


applyOTP :: String -> String -> String
applyOTP otp plaintext =
  let
    encryptedByte = applyOTP' otp plaintext
  in
    map byteToChar encryptedByte

--
applyOTP' :: String -> String -> Bytes
applyOTP' otp plaintext =  map xorPairOfBytes $ zip otpBytes plaintextBytes
  where otpBytes = map charToByte otp
        plaintextBytes = map charToByte plaintext
        xorPairOfBytes (left, right) = xorByte left right
--}
