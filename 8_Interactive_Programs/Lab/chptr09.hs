-- !!! run ghci: > ghci -i../../7_Functional_Parsers/Lab
module Chptr09 where

import           Chptr08
import           Parsing
import           Prelude   hiding (getLine, putStr, putStrLn)
import           System.IO hiding (getLine, putStr, putStrLn)
import           System.Posix.Unistd(sleep)

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
quit = do goto (1, 1)
          cls

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

evaluate :: String -> IO ()
evaluate xs = case parse expr xs of
                   [(n, "")] -> calc (show n)
                   [(_, re)] -> do goto (3 + min 13 (length xs) - length re, 2)
                                   beep
                                   getCh
                                   calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

runCalc :: IO ()
runCalc = do cls
             showbox
             clear


width :: Int
width = 5

height :: Int
height = 5

-- type Board = [Pos]
--            = [(Int, Int)]

-- glider :: Board
glider :: [(Int, Int)]
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: [(Int, Int)] -> IO ()
showcells b = seqn [ writeAt p "o" | p <- b ]

redrawCells :: [(Int, Int)] -> [(Int, Int)] -> IO ()
redrawCells pb nb = seqn (erase ++ draw)
                    where
                      erase = [ writeAt p " " | p <- pb, not . elem p $ nb ]
                      draw  = [ writeAt p "o" | p <- nb, not . elem p $ pb ]

isAlive :: [(Int, Int)] -> (Int, Int) -> Bool
isAlive b p = elem p b

isEmpty :: [(Int, Int)] -> (Int, Int) -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: (Int, Int) -> [(Int, Int)]
neighbs (x, y) = map wrap [(x - 1, y - 1), (x, y - 1),
                           (x + 1, y - 1), (x - 1, y),
                           (x + 1, y), (x - 1, y + 1),
                           (x, y + 1), (x + 1, y + 1)]

wrap :: (Int, Int) -> (Int, Int)
wrap (x, y) = (((x - 1) `mod` width) + 1,
               ((y - 1) `mod` height) + 1)

liveNeighbs :: [(Int, Int)] -> (Int, Int) -> Int
liveNeighbs b = length . filter (isAlive b) . neighbs

survivors :: [(Int, Int)] -> [(Int, Int)]
survivors b = [ p | p <- b, elem (liveNeighbs b p) [2, 3] ]

-- births :: [(Int, Int)] -> [(Int, Int)]
-- births b = [ (x, y) | x <- [1..width],
--                       y <- [1..height],
--                       isEmpty b (x, y),
--                       liveNeighbs b (x, y) == 3 ]

births :: [(Int, Int)] -> [(Int, Int)]
births b = [ p | p <- rmDups (concat (map neighbs b)),
                 isEmpty b p,
                 liveNeighbs b p == 3 ]

rmDups :: Eq a => [a] -> [a]
rmDups [] = []
rmDups (x:xs) = x : rmDups(filter (/=x) xs)

nextGen :: [(Int, Int)] -> [(Int, Int)]
nextGen b = survivors b ++ births b

life :: [(Int, Int)] -> IO ()
life b = do let nb = nextGen b
            redrawCells b nb
            goto (width + 1, height + 1)
            sleep 1
            life nb

-- life b = do cls
--             showcells b
--             wait 500
--             life (nextGen b)

wait :: Int -> IO ()
wait n = seqn [ return () | _ <- [1..n] ]

-- Exercises

-- getLine :: IO String
-- getLine = do x <- getChar
--              if x == '\n' then
--                 return []
--               else
--                 do xs <- getLine
--                    return (x:xs)

readLine :: String -> IO String
readLine xs =
  do c <- getCh
     case c of
          '\DEL' -> do putStr "\ESC[1D \ESC[1D"
                       cs <- readLine (if length xs == 0 then xs else init xs)
                       return cs
          '\n'   -> do putChar c
                       return xs
          _      -> do putChar c
                       cs <- readLine (xs ++ [c])
                       return cs




































