module FuelDepot where

import Data.Char
import Data.List

ascending :: Int -> Bool
ascending x = let xs = show x in and $ zipWith (<=) xs (tail xs)

groupCriteria n = any ((>= 2) . length) (group $ show n)

enhancedGroupCriteria n = any ((== 2) . length) (group $ show n)

replicatedInt n digit = foldl (\acc pow -> acc + (digit * (10 ^ pow))) 0 [0..(n-1)]

increment n = increment' (n+1) 0

increment' n c | n == 0          = 0
               | n `mod` 10 == 0 = increment' (n `div` 10) (c + 1)
               | otherwise       = (n * (10 ^ c)) + replicatedInt c (n `mod` 10)

ensureAscending :: Int -> Int
ensureAscending n = read $ ensureAscending' '0' (show n)
  where ensureAscending' n [] = []
        ensureAscending' n (x:xs) = if x >= n then x : ensureAscending' x xs else n : ensureAscending' n xs

passwordsInRange start end = passwordsInRange' (ensureAscending start) end
  where
    passwordsInRange' start end | start < end = start : passwordsInRange' (increment start) end
                                | otherwise   = [end]
