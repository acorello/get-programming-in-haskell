import Data.List

names = [("Ian", "Curtis")
        ,("Bernard", "Sumner")
        ,("Peter", "Hook")
        ,("Stephen", "Morris")]
  
type Chars = [Char]

compareLastNames :: (Chars, Chars) -> (Chars, Chars) -> Ordering
compareLastNames name1 name2 =
  let
    lastName1 = snd name1
    lastName2 = snd name2
  in
    compare lastName1 lastName2
