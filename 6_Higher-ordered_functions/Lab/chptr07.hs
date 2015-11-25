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

myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldr (\x -> (&&) (p x)) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldl (\res x -> res || (p x)) False

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x xs -> if p x then [x] ++ xs else [] ++ []) []
-- myTakeWhile _ [] = []
-- myTakeWhile p (x:xs) = if p x then [x] ++ myTakeWhile p xs else [] ++ []

myDropWhile :: (a -> Bool) -> [a] -> [a]
-- myDropWhile p = foldr (\x xs -> if p x then xs else x : xs) []
myDropWhile _ [] = []
myDropWhile p (x:xs) = if p x then myDropWhile p xs else x : xs

myMap3 :: (a -> b) -> [a] -> [b]
myMap3 f = foldr (\x -> (:) (f x)) []

myFilter3 :: (a -> Bool) -> [a] -> [a]
myFilter3 p = foldr (\x -> if p x then (:) x else id) []

dec2int :: [Int] -> Int
dec2int xs = (fst . foldl (\res x -> (fst res + x * snd res, snd res `div` 10)) (0, 10^(length xs - 1))) xs

dec2int2 :: [Int] -> Int
dec2int2 = foldl (\x y -> 10 * x + y) 0

myCurry :: ((a,b) -> c) -> (a -> b -> c)
myCurry f = \x y -> f (x,y)

myUncurry :: (a -> b -> c) -> ((a,b) -> c)
myUncurry f = \pair -> f (fst pair) (snd pair)

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin2 :: Int -> [Bit]
int2bin2 = unfold (== 0) (`mod` 2) (`div` 2)

chop82 :: [Bit] -> [[Bit]]
chop82 = unfold (null) (take 8) (drop 8)

myMap4 :: (a -> b) -> [a] -> [b]
myMap4 f = unfold (null) (f . head) (tail)

myIterate :: (a -> a) -> a -> [a]
myIterate f = unfold (\_ -> False) (id) (f)

addParity :: [Bit] -> [Bit]
addParity [] = []
addParity xs = xs ++ [(sum xs `mod` 2)]

checkParity :: [Bit] -> [Bit]
checkParity xs | sum xs `mod` 2 == 0 = init xs
               | otherwise           = error "Parity wrong"

encode2 :: String -> [Bit]
encode2 = concat . map (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold (null) (take 9) (drop 9)

decode2 :: [Bit] -> String
decode2 = map (chr . bin2int2) . map (checkParity) . chop9

transmit2 :: String -> String
transmit2 = decode2 . channel . encode2

channel2 :: [Bit] -> [Bit]
channel2 = tail

transmit3 :: String -> String
transmit3 = decode2 . channel2 . encode2

