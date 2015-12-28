factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x:(xs +++ ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (_:xs) = myDrop (n - 1) xs

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

-- Mutual recursion:
myEven :: Int -> Bool
myEven 0 = True
myEven n = myOdd (n - 1)

myOdd :: Int -> Bool
myOdd 0 = False
myOdd n = myEven (n - 1)

(^^^) :: Int -> Int -> Int
0 ^^^ _ = 0
1 ^^^ _ = 1
_ ^^^ 0 = 1
x ^^^ 1 = x
x ^^^ y = x * (x ^^^ (y - 1))

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd [x] = x
myAnd (True:xs) = myAnd xs
myAnd (False:xs) = False

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs 

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate 1 x = [x]
myReplicate n x = x : myReplicate (n - 1) x

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) | x == y    = True
                | otherwise = myElem x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = msort (fst halves) `merge` msort (snd halves)
           where
             halves = (take l xs, drop l xs)
             l      = length xs `div` 2

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myTake :: Int -> [a] -> [a]
myTake 0 xs = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n - 1) xs

myLast :: [a] -> a
myLast [x]    = x
myLast (_:xs) = myLast xs

