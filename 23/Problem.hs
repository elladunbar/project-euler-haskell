import Data.List

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- numbers < n which divide evenly into n
properDivisors :: Int -> [Int]
properDivisors n = nub divisors
  where
    divisors = 1 : concatMap (\m -> let (quotient, modulus) = divMod n m in if modulus == 0 then [m, quotient] else []) [2 .. isqrt n]

solution :: Int -> Int
solution n = sum $ [1 .. n] \\ abundantSums
  where
    abundantNumbers = filter (\m -> sum (properDivisors m) > m) [1 .. n `div` 2]
    abundantSums = map sum $ [(i, j) | i <- abundantNumbers, j <- abundantNumbers]

main = do
    print $ solution 28123
