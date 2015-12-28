module Chptr13 where

-- 1. Base case
-- 2. Inductive case (if holds for n, prove (next n))

-- 1. Apply definition
-- 2. Apply induction
-- 3. Unapply definition

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

-- prove : length (replicate n x) = n

-- 1. Base case:

-- length (replicate 0 x) =
--   length ([]) =
--     0

-- 2. Inductive case (state that holds for n):

-- length (replicate (n + 1) x) =
--   length (x : replicate n x) =
--     1 + length (replicate n x) =
--       1 + n =
--         n + 1

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

newtype Stack = [Int]
type    Code  = [Op]
data    Op    = PUSH Int | ADD

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

exec (comp e) [] = [eval e]
exec (comp e) s  = eval e : s

-- exec (comp (Val n)) s =
--   exec [PUSH n] s =
--     exec [] (n : s) =
--       n : s =
--         eval (Val n) : s

-- exec (comp (Add x y)) s =
--   exec (comp x ++ comp y ++ [ADD]) s =
--     exec (comp y ++ [ADD]) (exec (comp x) s) =
--       exec (comp y ++ [ADD]) (eval x : s) =
--         exec ([ADD]) (exec (comp y) (eval x : s)) =
--           exec ([ADD]) (eval y : eval x : s) =
--             exec [] ((eval x + eval y) : s) =
--               eval (Add x y) s

-- given: 
--   add :: Nat -> Nat -> Nat
--   add Zero m = m
--   add (Succ n) m = Succ (add n m)

-- prove:
--   add n (Succ m) = Succ (add n m)

-- add Zero (Succ m) =
--   Succ m =
--     Succ (add Zero m)

-- add (Succ n) (Succ m) =
--   Succ (add n (Succ m)) =
--     Succ (Succ (add n m)) =
--       Succ (add (Succ n) m)


-- given:
--   add n Zero = n

-- prove:
--   add n m = add m n


-- add Zero m =
--   m =
--     add m Zero

-- add (Succ n) m =
--   Succ (add n m) =
--     Succ (add m n) =
--       add m (Succ n)

-- given:
--   all p [] = True
--   all p (x:xs) = p x && all p xs

-- prove:
--   all (==x) (replicate n x) == True

-- all (==x) (replicate 0 x) =
--   all (==x) [] =
--     True

-- all (==x) (replicate (n + 1) x) =
--   all (==x) (x : replicate n x) =
--     (x == x) && all (==x) (replicate n x) =
--       True && True =
--         True

-- given:
--   [] ++ ys     = ys
--   (x:xs) ++ ys = x : (xs ++ ys)

-- prove:
--   xs ++ [] = xs

-- [] ++ [] =
--   []

-- (x:xs) ++ [] =
--   x : (xs ++ []) =
--     x : xs

-- prove:
--   xs ++ (ys ++ xs) = (xs ++ ys) ++ zs

-- [] ++ (ys ++ zs) =
--   ys ++ xs =
--     ([] ++ ys) ++ zs

-- (x:xs) ++ (ys ++ zs) =
--   x : (xs ++ (ys ++ xs)) =
--     x : ((xs ++ ys) ++ zs) =
--       x : (xs ++ ys) ++ zs =
--         (x:xs ++ ys) ++ zs














