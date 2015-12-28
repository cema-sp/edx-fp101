module Chptr11 where

-- example: [1,3,7,10,25,50] -> 765

data Op = Add | Sub | Mul | Div | Exp
          deriving (Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

-- only natural numbers, no 0
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0
valid Exp x y = y /= 1

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x >  y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0
valid' Exp x y = y /= 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr
            -- deriving (Show)

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = parth l
                     ++ show o 
                     ++ parth r
                     where
                       parth (Val n) = show n
                       parth x = "(" ++ show x ++ ")"

  showList [] _     =  ""
  showList (x:xs) f = show x ++ "\n" ++ showList xs f

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- retun list as Maybe (empty -> failure)
eval :: Expr -> [Int]
eval (Val n) = [ n | n > 0 ]
eval (App o l r) = [ apply o x y | x <- eval l
                                 , y <- eval r
                                 , valid o x y ]

-- eval' :: Expr -> [Int]
-- eval' (Val n) = [ n | n > 0 ]
-- eval' (App o l r) = [ apply o x y | x <- eval l
--                                  , y <- eval r
--                                  , valid' o x y ]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

choices' :: [a] -> [[a]]
choices' xs = [ ps | ss <- subs xs
                   , ps <- perms ss ]

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] ys     = True
isChoice (x:xs) ys = elem x ys && isChoice xs (remove x ys)
                     where
                       remove q ps = [ p | p <- ps, p /= q ]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [ (x:ls, rs) | (ls, rs) <- split xs ]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [ e | (ls, rs) <- split ns
                , l <- exprs ls
                , r <- exprs rs
                , e <- combine l r ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

solutions  :: [Int] -> Int -> [Expr]
solutions ns n = [ e | ns' <- choices ns
                     , e <- exprs ns'
                     , eval e == [n] ]


type Result = (Expr, Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [ (Val n, n) | n > 0 ]
results ns  = [ res | (ls, rs) <- split ns
                    , lx <- results ls
                    , ry <- results rs
                    , res <- combine' lx ry ]

results' :: [Int] -> [Result]
results' []  = []
results' [n] = [ (Val n, n) | n > 0 ]
results' ns  = [ res | (ls, rs) <- split ns
                     , lx <- results' ls
                     , ry <- results' rs
                     , res <- combine'' lx ry ]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops
                                                   , valid o x y ]

combine'' :: Result -> Result -> [Result]
combine'' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops
                                                    , valid' o x y ]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [ e | ns' <- choices ns
                      , (e, m) <- results ns'
                      , m == n ]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [ e | ns' <- choices ns
                       , (e, m) <- results' ns'
                       , m == n ]

solutionsNear :: [Int] -> Int -> [Result]
solutionsNear ns n = filter ((==m) . snd) $ slns
                     where
                       slns = [ (e, abs (m - n)) | ns' <- choices ns
                                                 , (e, m) <- results' ns' ]
                       m   = minimum . map (snd) $ slns

simplicityTable :: [(Op, Int)]
simplicityTable = [(Add, 1)
                  ,(Sub, 2)
                  ,(Mul, 3)
                  ,(Div, 4)
                  ,(Exp, 5)
                  ]

simplicityOfOp :: Op -> Int
simplicityOfOp o = head [ s | (o', s) <- simplicityTable, o' == o ]

simplicity :: Expr -> Int
simplicity (Val _) = 0
simplicity (App o l r) = simplicity l + simplicityOfOp o + simplicity r

orderBySimplicity :: [Expr] -> [Expr]
orderBySimplicity [] = []
orderBySimplicity (x:xs) = orderBySimplicity l ++ [x] ++ orderBySimplicity g
                           where
                            l  = [ y | y <- xs, simplicity y < sx ]
                            g  = [ y | y <- xs, simplicity y >= sx ]
                            sx = simplicity x






