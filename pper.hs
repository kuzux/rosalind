numPerms :: Int -> Int -> Int
numPerms n k = product [n-k+1..n] `mod` 1000000
