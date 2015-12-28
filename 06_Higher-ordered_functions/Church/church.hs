-- type Church = (a -> a) -> a -> a
--                ^^^^^^ - successor function
--                           ^ - zero number
-- num N = N applications of successor function

zero = \s z -> z
one  = \s z -> s z
-- two  = \s z -> s (s z)
--      = \s z -> (s . s) z
--      = \s   ->  s . s
two  = \s   -> s . s

c2i x = x (+1) 0
--        ^^^^ - successor
--             ^ - zero number

c2s x = x ('*':) ""

-- x' = c2i x
-- y' = c2i y

-- x' + y' = c2i (add x y)
--         = c2i x + c2i y
--         = x (+1) 0 + c2i y
--         = x (+1) (c2i y)
--         = x (+1) (y (+1) 0)
--         = (\s z -> x s (y s z)) (+1) 0
--         = (\s   -> x s . y s) (+1) 0
-- add x y = (\s   -> x s . y s)

add x y = \s -> x s . y s
-- mul x y = \s z -> x (y s) z
mul x y = x . y
