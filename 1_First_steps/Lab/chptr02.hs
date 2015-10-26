-- Average
myaverage ns = div (sum ns) (length ns)

-- Fixed function
myN = a `div` length xs
      where
        a = 10
        xs = [1..5]

myLast (xs) = xs !! (length xs - 1) 

myLast2 (xs) = head (reverse xs)

myInit (xs) = take (length xs - 1) xs

myInit2 (xs) = reverse (tail (reverse xs))

