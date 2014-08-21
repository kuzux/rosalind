fact :: Integer -> Integer
fact n = product [1..n]

comb :: Integer -> Integer -> Integer
comb n k = (fact n) `div` (fact k * fact (n-k))

sumCombs :: Integer -> Integer -> Integer
sumCombs m n = (sum $ map (comb n) [m..n]) `mod` 1000000
