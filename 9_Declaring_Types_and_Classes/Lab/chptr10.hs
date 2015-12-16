module Chptr10 where

import           Prelude hiding (Down, Left, Right, Up)

-- type String = [Char]

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k' ]

-- data Bool = False | True
--             ^^^^^ , ^^^^ - constructors

type Pos = (Int, Int)
data Move = Left | Right | Up | Down deriving(Show)

move :: Move -> Pos -> Pos
move Left  (x, y) = (x - 1, y)
move Right (x, y) = (x + 1, y)
move Up    (x, y) = (x, y - 1)
move Down  (x, y) = (x, y + 1)

moves :: [Move] -> Pos -> Pos
moves [] p     = p
moves (m:ms) p = moves ms (move m p)

flip :: Move -> Move
flip Left  = Right
flip Right = Left
flip Up    = Down
flip Down  = Up

data Shape = Circle Float | Rect Float Float deriving(Show)

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- data Maybe a = Nothing | Just a
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv m n = Just (m `div` n)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

data Nat = Zero | Succ Nat deriving(Show)

-- Succ (Succ (Succ Zero)) = 1 + (1 + (1 + 0))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult Zero _ = Zero
mult (Succ n) m = add (mult n m) m

data List a = Nil | Cons a (List a) deriving(Show)
--    empty - ^^^   (:)  x  xs
len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

data Tree = Leaf Int | Node Tree Int Tree
            deriving(Show)
t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs' :: Int -> Tree -> Bool
occurs' m (Leaf n)     = m == n
occurs' m (Node l n r) = m == n
                        || occurs m l
                        || occurs m r

flatten :: Tree -> [Int]
flatten (Leaf n)     = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a -> Ordering

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
  | m == n    = True
  | m <  n    = occurs m l
  | otherwise = occurs m r

occurs'' :: Int -> Tree -> Bool
occurs'' m (Leaf n)     = m == n
occurs'' m (Node l n r) = case compare m n of
                              EQ -> True
                              LT -> occurs' m l
                              GT -> occurs' m r

lenT :: Tree -> Int
lenT (Leaf n) = 1
lenT (Node l n r) = lenT l + 1 + lenT r

-- instance Show Tree where
--   -- showPrec p t = id
--   showList []     = \_ -> ""
--   showList (t:ts) = \_ -> show t ++ show ts
--   show (Leaf n)     = show n
--   show (Node l n r) = replicate (lenT l) ' '
--                       ++ show n
--                       ++ replicate (lenT r) ' '
--                       ++ "\n"
--                       ++ show (l:r:[])

data BTree = BLeaf Int | BNode BTree BTree
             deriving(Show)

leaves :: BTree -> Int
leaves (BLeaf _)   = 1
leaves (BNode l r) = leaves l + leaves r

balanced :: BTree -> Bool
balanced (BLeaf _) = True
balanced (BNode l r) = if diff >= -1 && diff <= 1 then True else False
                       where diff = leaves l - leaves r

balance :: [Int] -> BTree
balance []  = error "Empty list given"
balance [x] = BLeaf x
balance xs  = BNode (balance l) (balance r)
              where
                l = take m xs
                r = drop m xs
                m = length xs `div` 2

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Eqv Prop Prop
            deriving(Show)

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Eqv (And (Not (Var 'A')) (Not (Var 'B'))) (Not (Or (Var 'A') (Var 'B')))

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Eqv p q)   = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Eqv p q)   = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n - 1)

-- bools 3 = [[x,y,z] | x <- [True, False], y <- [True, False], z <- [True, False]]

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where
             vs     = rmdups (vars p)
             rmdups = foldl (\xs x -> if elem x xs then xs else xs ++ [x]) []

isTaut :: Prop -> Bool
-- isTaut p = all (==True) (map (\s -> eval s p) (substs p))
isTaut p = and [ eval s p | s <- substs p ]


data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y

type Cont = [Op]
data Op   = EVAL Expr | ADD Int
-- controll stack

evalE :: Expr -> Cont -> Int
evalE (Val n) c   = execC c n
evalE (Add x y) c = evalE x (EVAL y : c)

execC :: Cont -> Int -> Int
execC [] n = n
execC (EVAL y : c) n = evalE y (ADD n : c)
execC (ADD n : c) m  = execC c (n + m)

valueE :: Expr -> Int
valueE e = evalE e []










