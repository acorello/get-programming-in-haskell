type FirstName = String

type LastName = String

type MiddleName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName
          | FirstNameWithTwoInits FirstName Char Char

instance Show Name where
  show (Name firstname lastname) =
    "'" ++ firstname ++ " " ++ lastname ++ "'"
  show (NameWithMiddle firstname middle lastname) =
    "'" ++ firstname ++ " " ++ middle ++ " " ++ lastname ++ "'"
  show (TwoInitialsWithLast f m lastname) =
    "'" ++ [f] ++ " " ++ [m] ++ " " ++ lastname ++ "'"
  show (FirstNameWithTwoInits firstname m l) =
    "'" ++ firstname ++ " " ++ [m] ++ " " ++ [l] ++ "'"

data Creator = Person Name
             | Band String
             deriving (Show)

data StoreItem = Book
                 { bookCreator :: Creator
                 , bookIsbn    :: String
                 , bookTitle   :: String
                 , bookYear    :: Int
                 , bookPrice   :: Double
                 }
               | VinylRecord
                 { recordCreator :: Creator
                 , recordTitle   :: String
                 , recordYear    :: Int
                 , recordPrice   :: Double
                 }
               | Toy
                 { toyName        :: String
                 , toyDescription :: String
                 , toyPrice       :: Double
                 }
               deriving (Show)

price :: StoreItem -> Double
price Book {bookPrice = p} = p
price VinylRecord {recordPrice = p} = p
price Toy {toyPrice = p} = p

madeBy :: StoreItem -> String
madeBy Book {bookCreator = c} = show c
madeBy VinylRecord {recordCreator = c} = show c
madeBy Toy _ = "N/A"

c = Toy
  { toyName = "Toy"
  , toyDescription = "Great Toy"
  , toyPrice = 10.60
  }

b = Book
  { bookCreator = Person $ Name "Joe" "Blogg"
  , bookIsbn = ""
  , bookTitle = "Joy"
  , bookYear = 1980
  , bookPrice = 50.0
  }
