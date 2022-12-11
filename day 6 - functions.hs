-- safetail using conditional
safetailC :: [a] -> [a]
safetailC (a) =
  if null a
    then []
    else tail a

-- safetail using guarded equations
safetailG :: [a] -> [a]
safetailG (a)
  | null a = []
  | otherwise = tail a

-- safetail using pattern matching
safetailP :: [a] -> [a]
safetailP ([]) = []
safetailP (a)  = tail a

-- or implementation using pattern matching
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _         = True

-- and implementation using conditionals
(##) :: Bool -> Bool -> Bool
(##) a b =
  if a == True
    then b
    else False
