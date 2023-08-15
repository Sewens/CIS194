{-
Homework1 by Sewen.
-}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
        | null xs = []
        |otherwise = [if even i then x*2 else x | (i,x) <- listWithIndex]
            where listWithIndex = zip [0..] xs