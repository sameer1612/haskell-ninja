-- check if all booleans in a list are true.
and1 :: [Bool] -> Bool
and1 []     = True
and1 (x:xs) = x && and1 xs

-- concat lists into a single list.
concat1 :: [[a]] -> [a]
concat1 []       = []
concat1 (xs:xss) = xs ++ concat1 xss

-- generate list of n elements with value x.
replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 n x = x : replicate1 (n - 1) x

-- select i-th element of the list.
(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0  = x
(!!!) (_:xs) i = xs !!! (i - 1)

-- find if value exists in the list.
elem1 :: Eq a => a -> [a] -> Bool
elem1 el []     = False
elem1 el (x:xs) = el == x || elem1 el xs

-- insert element in sorted list.
insert :: Int -> [Int] -> [Int]
insert el [] = [el]
insert el (x:xs) =
  if el < x
    then el : x : xs
    else x : insert el xs

-- insertion sort.
isort :: [Int] -> [Int]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- merge sorted lists.
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x <= y
    then x : merge xs (y : ys)
    else y : merge (x : xs) ys

-- merge sort.
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where
    ys = take (length xs `div` 2) xs
    zs = drop (length xs `div` 2) xs

main :: IO ()
main = do
  putStrLn $ unwords (["and1: ", show (and1 [True, True, False])])
  putStrLn $ unwords (["concat1: ", show (concat1 [[1], [2, 3]])])
  putStrLn $ unwords (["replicate1: ", show (replicate1 5 "*")])
  putStrLn $ unwords (["!!!: ", show ([2, 4, 6] !!! 1)])
  putStrLn $ unwords (["elem1: ", show (elem1 4 [2, 4, 6])])
  putStrLn $ unwords (["isort: ", show (isort [2, 3, 1])])
  putStrLn $ unwords (["msort: ", show (msort [2, 3, 1])])
