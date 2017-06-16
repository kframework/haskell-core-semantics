module BouncyNumbers where

digits :: Integer -> [Integer]
digits n = reverse $ inverseDigits n
           where inverseDigits n'
                   | n' == 0 = []
                   | otherwise = (n' `mod` 10) : inverseDigits (n' `div` 10)

differences :: [Integer] -> [Integer]
differences []     = []
differences [_] = []
differences (x:xs) = x - head xs : differences xs

bouncy :: Integer -> Bool
bouncy n = any (> 0) diffs && any (< 0) diffs
           where diffs = differences $ digits n

countBouncyNumbers :: Integer
countBouncyNumbers  = iterateCount 0 0 1
                      where
                            iterateCount :: Integer -> Integer -> Integer
                                         -> Integer
                            iterateCount iter count num
                              | ratio == (0.99 :: Double) = num - 1
                              | otherwise = if bouncy num
                                            then iterateCount (iter + 1) (count + 1) (num + 1)
                                            else iterateCount (iter + 1) count (num + 1)
                              where
                                ratio = fromIntegral count / fromIntegral iter

result :: Integer
result = countBouncyNumbers
