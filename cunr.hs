numTrees :: Integer -> Integer
numTrees n = (fact $ 2*n-4) `div` ((fact $ n-2)*2^(n-2))
    where fact n = product [1..n]

