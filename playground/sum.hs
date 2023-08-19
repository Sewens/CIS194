{-
Leetcode quiz: 2235
add two number
-}

sum2 :: Integer -> Integer -> Integer
sum2 a b
    | b == 0 = a
    |otherwise = 1 + (sum2 a (b-1))


    