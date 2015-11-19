
riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x, y) <- zip xs ys]

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0 && x `div` y >= 1

divisors :: Int -> [Int]
divisors x = [ y | y <- [1..x], x `divides` y ]

