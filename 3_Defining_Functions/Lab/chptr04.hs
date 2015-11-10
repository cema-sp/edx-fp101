isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9')

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

recip_2 :: Fractional a => a -> a
recip_2 n = 1 / n

myAbs :: Int -> Int
myAbs n = if n >= 0 then n else -n

mySignum :: Int -> Int
mySignum n = if n > 0 then 1 else
               if n == 0 then 0 else -1

myAbs_2 :: Int -> Int
myAbs_2 n | n >= 0    = n
          | otherwise = -n

mySignum_2 :: Int -> Int
mySignum_2 n | n > 0     = 1
             | n == 0    = 0
             | otherwise = -1

myNot :: Bool -> Bool
myNot False = True
myNot True  = False

myAnd :: Bool -> Bool -> Bool
True  `myAnd` True  = True
True  `myAnd` False = False
False `myAnd` True  = False
False `myAnd` False = False

myAnd_2 :: Bool -> Bool -> Bool
True `myAnd_2` True = True
_ `myAnd_2` _       = False

myAnd_3 :: Bool -> Bool -> Bool
True  `myAnd_3` b = b
False `myAnd_3` _ = False

myAnd_4 :: Bool -> Bool -> Bool
a `myAnd_4` b | a == b    = b
              | otherwise = False

myFst :: (a, b) -> a
myFst (x, _) = x

mySnd :: (a, b) -> b
mySnd (_, y) = y

threeWithA :: [Char] -> Bool
threeWithA ['a', _, _] = True
threeWithA _           = False

startWithA :: [Char] -> Bool
startWithA ('a':_) = True
startWithA _       = False

myNull :: [a] -> Bool
myNull [] = True
myNull (_:_)  = False

myHead :: [a] -> a
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

-- pred :: Int -> Int
-- pred 0 = 0
-- pred (n+1) = n

myAdd :: Num a => a -> a -> a
myAdd = \x -> (\y -> x + y)

myConst :: a -> b -> a
myConst x _ = x

myConst_2  :: a -> (b -> a)
myConst_2 x = \_ -> x

myOdds_1 :: Int -> [Int]
myOdds_1 n = map f [0..n-1]
             where f x = x * 2 + 1

myOdds_2 :: Int -> [Int]
myOdds_2 n = map (\x -> x * 2 + 1) [0..n-1]

