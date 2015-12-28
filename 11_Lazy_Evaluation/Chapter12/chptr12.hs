module Chptr12 where

-- innermost evaluation === call-by-value (values passed)
-- outermost evaluation === call-by-name (names passed (pointers ?))

primes :: [Int]
primes = sieve [2..]
         where
           sieve (p:ps) = p : sieve [ x | x <- ps
                                        , x `mod` p /= 0 ]

-- f $ x
-- ^ - control part
--     ^ - data part

-- f $! x
--   ^^ - strict evaluation (call-by-value), but it doesn't entirely evaluate

sumwith :: Int -> [Int] -> Int
--
sumwith v []     = v
-- sumwith v (x:xs) = sumwith (v + x) xs 
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ - eats much memory
sumwith v (x:xs) = (sumwith $! (v + x)) xs

fibs :: [Integer]
fibs = 0:1:[ x + x' | (x', x) <- zip fibs (tail fibs) ]

fib :: Int -> Integer
fib n = fibs !! n
-- fib n = head [ x | (x, i) <- zip fibs [0..]
--                  , i == n ]

fibGT1000 :: Integer
fibGT1000 = head . dropWhile (<1000) $ fibs
-- fibGT1000 = head . filter (>1000) $ fibs


data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving (Show, Eq)

repeatT :: a -> Tree a
repeatT n = Node (repeatT n) n (repeatT n)

takeT :: Int -> Tree a -> Tree a
takeT 0 _            = Leaf
takeT n Leaf         = Leaf
takeT n (Node l v r) = Node (takeT (n - 1) l) v (takeT (n - 1) r)

replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT
