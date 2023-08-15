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

{-
Execise4
Check valid bank id
Rules:
1. Double every digit on even index
2. Scatter all numbers into digits
3. Sum all digits and calculate the remainder when the sum is divided by 10
-}

validate :: Integer -> Bool
-- validate xs = sum (concat [toDigits x | x <- doubleEveryOther $ toDigits xs]) == 0
validate xs = sumOfDigits (foldl (++) [] [toDigits x | x <- doubleEveryOther $ toDigits xs]) `mod` 10 == 0

{-
Execise5
Hanoi tower
-}

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c
        | x == 1 = [(a,b)]
        | otherwise = (hanoi (x-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (x-1) c b a)

{-
Execise6
Hanoi tower with 4 pegs
Refer: https://dl.acm.org/doi/pdf/10.1145/126459.126460
Target: move all disk from peg1 to peg4
1. Keep k bottom disk in n-1 fixed in peg1
2. Move top n-k disk to peg2 by peg3 and peg4, make new sub-struct of sovling hanoi n-k a c d b
3. Move remained k disk to peg4 by peg3, just like 3-peg hanoi k a c d
4. Move top n-k disk from peg2 to peg4 using buffer peg1 and peg3 hanoi n-k b a c d
-}

{-
Always use peg2 as buffer
-}
-- type Peg = String
-- type Move = (Peg, Peg)
hanoiP3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiP3 x a b c
        | x == 1 = [(a,c)]
        | otherwise = (hanoi (x-1) a b c) ++ (hanoi 1 a b c) ++ (hanoi (x-1) b a c)

-- type Peg = String
-- type Move = (Peg, Peg)
hanoiP4 :: Integer -> Integer-> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiP4 x k a b c d
        | x <= 0 = []
        | x == 1 = hanoiP3 1 a b d
        | otherwise = (hanoiP4 (x-k) k a c d b) ++ (hanoiP3 k a c d) ++ (hanoiP4 (x-k) k b a c d)