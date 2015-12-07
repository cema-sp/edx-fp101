module Chptr08 where

-- import Prelude hiding (return)
import Parsing

-- type Parser = String -> Tree

-- type Parser = String -> (Tree, String)
--                                ^^^^^^ - remaining string

-- type Parser = String -> [(Tree, String)]
-- empty list == Failure

-- type Parser a = String -> [(a, String)]
--             ^ -polimorphic- ^

-- Parser :: a -> (String -> [(a, String)])

-- return :: a -> Parser a
-- return :: a -> (String -> [(a, String)])
-- return v = \inp -> [(v, inp)]

-- failure :: Parser a
-- failure :: (String -> [(a, String)])
-- failure = \_ -> []

-- item :: Parser Char
-- item :: (String -> [(Char, String)])
-- item = \inp -> case inp of
--                     []     -> []
--                     (x:xs) -> [(x, xs)]

-- parse :: Parser a -> String -> [(a, String)]
-- parse :: (String -> [(a, String)]) -> String -> [(a, String)]
-- parse p inp = p inp

-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
-- (>>=) :: (String -> [(a, String)]) -> (a -> (String -> [(b, String)])) -> (String -> [(b, String)])
-- p >>= f = \inp -> case parse p inp of
--                        []         -> []
--                        [(v, out)] -> parse (f v) out

-- 'then' operator: apply first, if OK - apply second, defined by result of first
-- (item Chptr08.>>= (\_ -> item)) "LOLZ" = [('O',"LZ")]
-- (item Chptr08.>>= (\v1 -> (item Chptr08.>>= (\v2 -> Chptr08.return (v1, v2))))) "LOLZ" = [(('L','O'),"LZ")]
-- same as: do { v1 <- item; v2 <- item; Chptr08.return (v1,v2) }

firstAndThird :: Parser (Char, Char)
firstAndThird = do x <- item
                   item
                   y <- item
                   return (x, y)

-- 'or else' operator: apply first, if fails - second
-- (+++) :: Parser a -> Parser a -> Parser a
-- p +++ q = \inp -> case parse p inp of
--                        []         -> parse q inp
--                        [(v, out)] -> [(v, out)]
