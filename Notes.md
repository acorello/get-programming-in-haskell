


Regarding Stackoverflow question:

> Why is `succ i` valid where `i :: Num a => a` (and not an `Enum a`)?

I'm thinking that `1` is stored as a polimorphic entity which can become
Int, Integer, Double.
But is this correct?

## Lesson 16

### Surprising Pattern Matching Behaviour

Non-Exhaustive pattern matching doesn't fail compilation when the pattern is nested.

I.e.

```haskell
data D = S String
       | I Int
       | C Char
       deriving (Show)

data T = T D String
       | U String

instance Show T where
  show (T (S string) last) = (show string) ++ last
  show (T (I int) _) = show int
 -- show (T (C char)) = show char -- if this is missing compilation does not fail
  show (U string) = string -- if this is missing compilation does fail
```

However the problem is fixed by implementing a function that takes the second argument only:

```haskell
instance Show T where
  show (T d last) = (showD d) ++ last
    where
      showD (S string) = string
      showD (I int) = show int
   -- showD (C char) = show char -- if this is missing compilation does fail
  show (U string) = string -- if this is missing compilation does fail
```

#### Records don't warn about non-exhaustive pattern matching

Given a data-type:

```haskell
data StoreItem = Book { ... , bookPrice :: Double }
               | Vinyl { ... , vinylPrice :: Double }
			   | Toy { ... , toyPrice :: Double }

price :: StoreItem -> Double
price Book {bookPrice = price} = price
price Toy {toyPrice = price} = price
```

I don't get a compilation error on the price function.
I expected it to fail because `Vinyl` case is not specified.

However if I enable `-Wincomplete-patterns` it does warn me about it.
