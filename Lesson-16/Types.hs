type FirstName = String

type LastName = String

type MiddleName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName
          | FirstNameWithTwoInits FirstName Char Char

instance Show Name where
  show (Name first last) =
    "'" ++ first ++ " " ++ last ++ "'"
  show (NameWithMiddle first middle last) =
    "'" ++ first ++ " " ++ middle ++ " " ++ last ++ "'"
  show (TwoInitialsWithLast f m last) =
    "'" ++ [f] ++ " " ++ [m] ++ " " ++ last ++ "'"
  show (FirstNameWithTwoInits first m l) =
    "'" ++ first ++ " " ++ [m] ++ " " ++ [l] ++ "'"

data Author = Author Name
            deriving (Show)

data Artist = Person Name
            | Band String
            deriving (Show)

data Creator = AuthorCreator Author
             | ArtistCreator Artist

instance Show Creator where
  show (AuthorCreator author) = "AuthorCreator" ++ showAuthor author
    where
    showAuthor (Author name) = show name
  show (ArtistCreator artist) = "ArstistCreator" ++ showArtist artist
    where
    showArtist (Person name) = show name
    showArtist (Band   name) = show name

data Book = Book { bookAuthor :: Creator
                 , bookIsbn   :: String
                 , bookTitle  :: String
                 , bookYear   :: Int
                 , bookPrice  :: Double
                 }
          deriving (Show)

data VinylRecord = VinylRecord { recordArtist  :: Creator
                               , recordTitle   :: String
                               , recordYear    :: Int
                               , recordPrice   :: Double
                               }
                 deriving (Show)

data CollectibleToy = CollectibleToy { toyName        :: String
                                     , toyDescription :: String
                                     , toyPrice       :: Double
                                     }
                    deriving (Show)

data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               deriving (Show)

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy

madeBy :: StoreItem -> String
madeBy (BookItem item) = show (bookAuthor item)
madeBy (RecordItem item) = show (recordArtist item)
madeBy (ToyItem _) = "N/A"
