import Data.Char

squaresTo :: Int -> [Int]
squaresTo n = [ x^2 | x <- [1..n]]

combinations :: [a] -> [b] -> [(a,b)]
combinations xs = \ys -> [ (x, y) | x <- xs, y <- ys]
-- same as: combinations xs ys = [ (x, y) | x <- xs, y <- ys]

combinations_2 :: Enum a => [a] -> [(a,a)]
combinations_2 xs = [ (x, y) | x <- xs, y <- [x..lastX]]
                    where lastX = last xs

myConcat :: [[a]] -> [a]
myConcat xss = [ x | xs <- xss, x <- xs ]

firsts :: [(a,b)] -> [a]
firsts ps = [ x | (x,_) <- ps ]

firsts_2 :: [(a,b)] -> [a]
firsts_2 ps = map fst [ p | p <- ps ]

myLength :: [a] -> Int
myLength xs = sum [ 1 | _ <- xs ]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [ x | x <- [2..n], prime x ]

findByKey :: Eq a => a -> [(a,b)] -> [b]
findByKey k ps = [ v | (k', v) <- ps, k' == k ]

adjPairs :: [a] -> [(a,a)]
adjPairs xs = zip xs (tail xs)

isSorted :: Ord a => [a] -> Bool
isSorted xs = and [ x <= y | (x, y) <- adjPairs xs ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i | (x', i) <- zip xs [0..l], x' == x ]
                 where l = length xs - 1

toUppercase :: [Char] -> [Char]
toUppercase cs = map (\c -> if c >= 'a' && c <= 'z' then toUpper c else c) cs
                 where toUpper = \c -> chr (ord c - (ord 'a' - ord 'A')) 

toLowercase :: [Char] -> [Char]
toLowercase cs = map (\c -> if c >= 'A' && c <= 'Z' then toLower c else c) cs
                 where toLower = \c -> chr (ord c + (ord 'a' - ord 'A')) 

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> [Char] -> [Char]
encode n xs = [ shift n x | x <- xs ]

decode :: Int -> [Char] -> [Char]
decode n xs = encode (-n) xs

crackTable :: [Float]
crackTable = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
              6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

freqs :: [Char] -> [Float]
freqs cs = [ percent (count c (toLowercase cs)) n | c <- letters ]
           where count c' cs' = length [ c'' | c'' <- cs', c'' == c' ]
                 n = length [ c' | c' <- toLowercase cs, elem c' letters ]
                 letters = ['a'..'z']

chiSquare :: [Float] -> [Float] -> Float
chiSquare os es = sum [ ((o - e)^2) / e | (o, e) <- zip os es ]

rotate :: Int -> [a] -> [a]
rotate k cs = drop k cs ++ take k cs

crackCode :: (Int -> [Char] -> [Char]) -> [Char] -> [Char]
crackCode decFn xs = decFn factor xs
                     where factor = head (positions (minimum chitab) chitab)
                           chitab = [ chiSquare (rotate n table') crackTable | n <- [0..25]]
                           table' = freqs xs

myEncode :: Int -> [Char] -> [Char]
myEncode k cs = map encChar cs
                where encChar c | elem c letters = head [ t | (c', t) <- dict, c' == c ]
                                | otherwise = c
                      dict      = zip letters translations
                      translations = shift k lowerLetters ++ shift k upperLetters
                      letters = lowerLetters ++ upperLetters
                      shift k' cs' = drop k' cs' ++ take k' cs'
                      lowerLetters = ['a'..'z']
                      upperLetters = ['A'..'Z']

myDecode :: Int -> [Char] -> [Char]
myDecode k cs = map decChar cs
                where decChar c | elem c letters = head [ t | (t, c') <- dict, c' == c ]
                                | otherwise = c
                      dict      = zip letters translations
                      translations = shift k lowerLetters ++ shift k upperLetters
                      letters = lowerLetters ++ upperLetters
                      shift k' cs' = drop k' cs' ++ take k' cs'
                      lowerLetters = ['a'..'z']
                      upperLetters = ['A'..'Z']

sumOfSquares :: Int -> Int
sumOfSquares n = sum [ i^2 | i <- [1..n] ]

myReplicate :: Int -> a -> [a]
myReplicate n x = [ x | _ <- [0..n-1]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- nums, y <- nums, z <- nums, x^2 + y^2 == z^2 ]
          where nums = [1..n]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (init (factors x)) == x ]

singleCom :: [a] -> [b] -> [(a,b)]
singleCom xs ys = concat [[(x,y) | y <- ys] | x <- xs]

positions_2 :: Eq a => a -> [a] -> [Int]
positions_2 x xs = findByKey x (zip xs [0..(length xs - 1)])

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ x * y | (x,y) <- zip xs ys ]

