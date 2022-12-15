-- find all pythagorian triplets in range n.
pyths :: Int -> [(Int, Int, Int)]
pyths n =
  [ (x, y, z)
  | x <- [1 .. n]
  , y <- [1 .. n]
  , z <- [1 .. n]
  , x ^ 2 + y ^ 2 == z ^ 2
  ]

-- find all factors of a number excluding itself.
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- find all perfect numbers in range n.
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (init (factors x)) == x]

-- find scalar product of two integer lists.
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

main :: IO ()
main = do
  putStrLn $ unwords (["pyths: ", show (pyths 5)])
  putStrLn $ unwords (["perfects: ", show (perfects 500)])
  putStrLn $ unwords (["scalar product: ", show (scalarProduct [1, 2] [4, 5])])
