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

-- sat :: (Char -> Bool) -> Parser Char
-- sat p = do x <- item
--            if p x then return x else failure

-- many :: Parser a -> Parser [a]
-- many p = many1 p +++ return []

-- many1 :: Parser a -> Parser [a]
-- many1 p = do v  <- p
--              vs <- many p
--              return (v:vs)

listOfNaturals :: Parser [Int]
listOfNaturals = do symbol "["
                    n  <- natural
                    ns <- many (do symbol ","
                                   natural)
                    symbol "]"
                    return (n : ns)

-- Grammar for arithmetic expression (WRONG)
-- expr ::= expr '+' expr | expr '∗' expr | '('expr')' | nat
-- nat ::= 0|1|2|···

-- Grammar for a expression (VALID)
-- expr ::= term '+' expr | term ::= term ('+' expr | -)
-- term ::= factor '*' term | factor ::= factor ('*' factor | -)
-- factor ::= '('expr')' | nat
-- nat ::= 0|1|2|···

-- expr :: Parser Int

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            +++
            do symbol "-"
               e <- expr
               return (t - e)
              +++
              return t

term :: Parser Int
term = do e <- expo
          do symbol "*"
             t <- term
             return (e * t)
            +++
            do symbol "/"
               t <- term
               return (e `div` t)
              +++
              return e

expo :: Parser Int
expo = do f <- factor
          do symbol "^"
             e <- expo
             return (f ^ e)
            +++
            return f

factor :: Parser Int
factor = natural +++ do symbol "("
                        e <- expr
                        symbol ")"
                        return e

eval :: String -> Int
eval xs = case parse expr xs of
               [(n, [])]  -> n
               [(_, out)] -> error ("unused input: " ++ out)
               []         -> error "invalid input"

int :: Parser Int
int = natural +++ do symbol "-"
                     n <- natural
                     return (-n)

integer :: Parser Int
integer =  token int

comment :: Parser ()
comment = do symbol "--"
             many (sat (/='\n'))
             char '\n'
             return ()

-- Grammar for subtraction:
-- subt ::= subt '-' subt | nat ::= subt '-' nat | nat ::= (subt '-' | -) nat
-- nat ::= 0 | 1 | 2 | ...

-- subt :: Parser Int
-- subt = do s <- subt
--           symbol "-"
--           n <- natural
--           return (s - n)
--          +++
--          natural

-- ^^^^^^^^^^^^^^^^^^^^^^^^ - not working

subt :: Parser Int
subt = do n <- natural
          ns <- many (do symbol "-"
                         natural)
          return (foldl (-) n ns)







