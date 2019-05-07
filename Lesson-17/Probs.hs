import Data.List (intercalate)

newtype Event
  = Event String
  deriving (Show)

instance Semigroup Event where
  (<>) (Event l) (Event r) =
    Event (mconcat [l, "-*-", r])


newtype Probability
  = Probability Double
  deriving (Show)

instance Semigroup Probability where
  (<>) (Probability p1) (Probability p2) =
    Probability (p1 * p2)

instance Monoid Probability where
  mempty = Probability 1.0


data EventStat
  = EventStat Event Probability

instance Semigroup EventStat where
  (<>) (EventStat e1 p1) (EventStat e2 p2) =
    EventStat e p
    where e = (e1 <> e2)
          p = (p1 <> p2)

instance Show EventStat where
  show (EventStat (Event name) (Probability value)) =
    mconcat [name, "|", show value]

newtype EventStats
  = EventStats [EventStat]

instance Semigroup EventStats where
  (<>) (EventStats lefts) (EventStats rights) =
    EventStats events'
    where
      events' = cartesian (<>) lefts rights

instance Num Probability where
  (+) (Probability l) (Probability r) =
    Probability (l+r)
  (-) (Probability l) (Probability r) =
    Probability (l-r)
  (*) (Probability l) (Probability r) =
    Probability (l*r)
  abs (Probability p) =
    Probability (abs p)
  signum (Probability p) =
    Probability (signum p)
  fromInteger i =
    Probability (fromInteger i)

instance Show EventStats where
  show (EventStats events) =
    intercalate "\n" $ map show events


probability :: EventStat -> Double
probability (EventStat _ (Probability p)) = p


divProbability :: Double -> EventStat -> EventStat
divProbability factor (EventStat event (Probability p)) =
  EventStat event (Probability (p / factor))


normalizeStats :: EventStats -> EventStats
normalizeStats (EventStats eventStats) =
  EventStats eventStats'
  where
    eventStats' = map normalize eventStats
    normalize = divProbability total
    total = sum $ map probability eventStats 
 

cartesian :: (x -> y -> z) -> [x] -> [y] -> [z]
cartesian f xs ys =
  zipWith f xs' ys'
  where
    ys' = cycle ys
    ysCount = length ys
    xs' = mconcat $ map (take ysCount . repeat) xs

-- fixtures
eventStat :: String -> Double -> EventStat
eventStat n p = 
  EventStat (Event n) (Probability p)

e1 = eventStat "e1" 0.7
e2 = eventStat "e2" 0.4
e3 = eventStat "e3" 0.6
e4 = eventStat "e4" 0.1

evs1 = EventStats [e1,e2]
evs2 = EventStats [e3,e4]
--}
