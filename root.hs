numUnrootedTrees :: Integer -> Integer
numUnrootedTrees n = (fact $ 2*n-4) `div` ((fact $ n-2)*2^(n-2))
    where fact n = product [1..n]

numRootedTrees :: Integer -> Integer
numRootedTrees n = (2*n-3) * numUnrootedTrees n
