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
               | Pamphlet
               { pamphletTitle :: String
               , pamphletDescription :: String
               , pamphletContact :: Name
               }
               deriving (Show)

price :: StoreItem -> Double
price Book {bookPrice = p} = p
price VinylRecord {recordPrice = p} = p
price Toy {toyPrice = p} = p
price Pamphlet {} = 0.0

madeBy :: StoreItem -> String
madeBy Book {bookCreator = c} = show c
madeBy VinylRecord {recordCreator = c} = show c
madeBy Toy {} = "N/A"
madeBy Pamphlet {} = "N/A"

toy1 = Toy
       { toyName = "Toy"
       , toyDescription = "Great Toy"
       , toyPrice = 10.60
       }

book1 = Book
        { bookCreator = Person $ Name "Joe" "Blogg"
        , bookIsbn = ""
        , bookTitle = "Joy"
        , bookYear = 1980
        , bookPrice = 50.0
        }


data Shape = Circle Double
           | Square Double
           | Rectangle Double Double
           
perimeter :: Shape -> Double
perimeter (Circle radius) = radius * 2.0 * pi
perimeter (Square side) = 4.0 * side
perimeter (Rectangle side1 side2) = 2.0 * (side1 + side2)

area :: Shape -> Double
area (Circle radius) = radius * radius * pi
area (Square side) = side * side
area (Rectangle side1 side2) = side1 * side2
