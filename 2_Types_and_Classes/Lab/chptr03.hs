mylen :: Num n => [n] -> Int
mylen [] = 0
mylen (x:xs) = 1 + mylen xs

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

dashed :: Show n => [n] -> String
dashed [] = "" :: String
dashed (x:xs) = show x ++ "-" ++ dashed xs

add' :: Int -> (Int -> Int)
add' x y = x + y

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

