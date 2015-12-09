-- !!! run ghci: > ghci -i../../7_Functional_Parsers/Lab
module Chptr09 where

import Parsing
import Chptr08
import System.IO hiding(getLine, putStr, putStrLn)
import Prelude hiding(getLine, putStr, putStrLn)

-- type IO = World -> World
-- IO takes initial 'state of the world' and returns modified one with side effects

-- type IO a = World -> (a, World)
--         ^ - returning value
-- ^^^^^^^^^ - 'action'

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- f >>= g = \world -> case f world of
--                          (v, world') -> g v world'

-- if f is successful, choose next (g v) and apply it

getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
                return []
              else
                do xs <- getLine
                   return (x:xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do putChar x
                   putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "Length: "
            putStr (show (length xs))
            putStrLn " characters" 


beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

-- type Pos = (Int, Int)
-- ^^^^^^^^ - position of character on the screen

-- goto :: Pos -> IO ()
goto :: (Int, Int) -> IO ()
goto (x, y) =
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- writeAt :: Pos -> String -> IO ()
writeAt :: (Int, Int) -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

box :: [String]
box  = ["+---------------+",
        "|               |",
        "+---+---+---+---+",
        "| q | c | d | = |",
        "+---+---+---+---+",
        "| 1 | 2 | 3 | + |",
        "+---+---+---+---+",
        "| 4 | 5 | 6 | - |",
        "+---+---+---+---+",
        "| 7 | 8 | 9 | * |",
        "+---+---+---+---+",
        "| 0 | ( | ) | / |",
        "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [ writeAt (1, y) xs | (y, xs) <- zip [1..length box] box]

display :: String -> IO ()
display xs = do writeAt (3, 2) (replicate 13 ' ')
                writeAt (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                process c xs
              else
                do beep
                   calc xs

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC"    = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n"       = evaluate xs
  | elem c "cC"        = clear
  | otherwise          = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

evaluate :: String -> IO ()
evaluate xs = case parse expr xs of
                   [(n, "")] -> calc (show n)
                   _ -> do beep
                           calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

runCalc :: IO ()
runCalc = do cls
             showbox
             clear











