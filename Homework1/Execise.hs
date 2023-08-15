{-
Execise1
Integer to list and reverse order list
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
Double element on even index
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
        | null xs = []
        |otherwise = [if even i then x*2 else x | (i,x) <- listWithIndex]
            where listWithIndex = zip [0..] xs

{-
Execise3
Sum Integer List
-}
sumOfDigits :: [Integer] -> Integer
-- sumOfDigits = foldl (+) 0
-- sumOfDigits = sum

sumOfDigits (x:xs)
        | null (x:xs) = 0     -- null list
        | null xs = x           --list with one elem
        | otherwise = x + sumOfDigits xs
