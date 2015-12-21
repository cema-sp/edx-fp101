module Lab5 where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

data Concurrent a = Concurrent ((a -> Action) -> Action)
--                               ^ - result      ^^^^^^ - current action
--                                    ^^^^^^ - future action
--                              ^^^^^^^^^^^^^ - continuation function

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

action :: Concurrent a -> Action
action (Concurrent c) = c (\_ -> Stop)


-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent (\_ -> Stop)


-- ===================================
-- Ex. 2
-- ===================================

-- atom :: IO a -> ((a -> Action) -> Action)
atom :: IO a -> Concurrent a
atom i = Concurrent (\c -> Atom (do a <- i
                                    return (c a)))
-- atom i = Concurrent (\c -> Atom (liftM c i))


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork c = Concurrent (\f -> Fork (action c) (f ()))

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent x) (Concurrent y) = Concurrent (\f -> Fork (x f) (y f))


-- ===================================
-- Ex. 4
-- ===================================

instance Functor Concurrent where
    fmap = liftM

instance Applicative Concurrent where
    pure x = Concurrent (\c -> c x)
    (<*>) = ap

instance Monad Concurrent where
    (Concurrent f) >>= g = Concurrent (\c -> f (\a -> (\(Concurrent g') -> g' c) (g a)))
    return = pure

-- instance Show (Concurrent a) where
--     show = show . action

-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roundRobin (x:xs) = case x of
                         (Atom i)   -> do a <- i
                                          roundRobin (xs ++ [a])
                         (Fork y z) -> roundRobin (xs ++ [y,z])
                         Stop       -> roundRobin xs

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

