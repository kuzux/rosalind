probRecessivePheno :: Double -> Double -> Double -> Double
probRecessivePheno k m n = 1 - ( bothParentsRecessive + 0.5 * recessiveAndHetero + 0.25 * bothParentsHetero )
    where totalInd             = k + m + n
          commonDenominator    = totalInd * (totalInd-1)
          bothParentsRecessive | n < 2     = 0
                               | otherwise = n*(n-1) / commonDenominator
          recessiveAndHetero   | m < 1 || n < 1 = 0
                               | otherwise      = 2*m*n / commonDenominator
          bothParentsHetero    | m < 2     = 0
                               | otherwise = m*(m-1) / commonDenominator
          -- nid x y              = fromIntegral $ (fromIntegral x) / (fromIntegral y)
