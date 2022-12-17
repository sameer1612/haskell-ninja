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
(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _         = True

-- and implementation using conditionals
(&&&) :: Bool -> Bool -> Bool
(&&&) a b =
  if a == True
    then b
    else False

main :: IO ()
main = do
  putStrLn $ unwords (["safetailC", show (safetailC [1, 2, 3])])
  putStrLn $ unwords (["safetailG", show (safetailG [1, 2, 3])])
  putStrLn $ unwords (["safetailP", show (safetailP [1, 2, 3])])
  putStrLn $ unwords (["|||", show (True ||| False)])
  putStrLn $ unwords (["&&&", show (True &&& False)])
