import Data.Char

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f [] = []
myMap2 f (x:xs) = f x : myMap2 f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = [x | x <- xs, p x]

myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 p [] = []
myFilter2 p (x:xs) | p x = x : myFilter2 p xs
                   | otherwise = myFilter2 p xs

-- foldr
-- f [] = v
-- f (x:xs) = x (+) f xs

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

mySum2 :: [Int] -> Int
mySum2 = foldr (+) 0

myProduct :: [Int] -> Int
myProduct = foldr (*) 1

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myLength :: [a] -> Int
myLength = foldr (\_ n -> n + 1) 0

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

-- foldl
-- f v [] = v
-- f v (x:xs) = f (v (+) x) xs
-- v - accumulator

mySum3 :: [Int] -> Int
mySum3 [] = 0
mySum3 (x:xs) = mySum3 xs + x

mySum4 :: [Int] -> Int
mySum4 = foldl (+) 0

myLength2 :: [a] -> Int
myLength2 = foldl (\n _ -> n + 1) 0

myReverse2 :: [a] -> [a]
myReverse2 = foldl (\xs x -> x : xs) []

sumSqrEven :: [Int] -> Int
sumSqrEven = sum . map (^2) . filter even

-- String transmitter

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [ w * b | (w, b) <- zip weights bits ]
               where weights = iterate (*2) 1

myBin2int :: [Bit] -> Int
myBin2int [] = 0
myBin2int (x:xs) = x * 2 ^ (length xs) + myBin2int xs

myBin2int_2 :: [Bit] -> [Int]
myBin2int_2 = foldr (\x xs -> x * 2 ^ (length xs) : xs) []

bin2int2 :: [Bit] -> Int
bin2int2 = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int2) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p = map f . filter p

