
double x = x + x

mysum [] = 0
mysum (x:xs) = x + mysum xs

myproduct [] = 1
myproduct (x:xs) = x * myproduct xs

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [ a | a <- xs, a <= x]
                 larger = [ b | b <- xs, b > x]

qsort2 [] = []
qsort2 (x:xs) = qsort2 larger ++ [x] ++ qsort2 smaller
               where
                 smaller = [ a | a <- xs, a <= x]
                 larger = [ b | b <- xs, b > x]

qsort3 [] = []
qsort3 (x:xs) = qsort3 larger ++ [x] ++ qsort3 smaller
               where
                 smaller = [ a | a <- xs, a < x]
                 larger = [ b | b <- xs, b > x]
