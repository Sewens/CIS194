{-
Execise1
-}
toDigits :: Integer -> [Integer]
toDigits x
        | x <=0 = []
        | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
        | x <=0 = []
        | otherwise = x`mod`10 : toDigitsRev (x`div`10)

{-
Execise2
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
        | null xs = []
        |otherwise = [if even i then x*2 else x | (i,x) <- listWithIndex]
            where listWithIndex = zip [0..] xs