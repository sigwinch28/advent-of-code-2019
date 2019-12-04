module FuelDepot where

import Data.List

digits :: Int -> [Int]
digits n = reverse $ digits' n
  where digits' n | n < 10    = [n]
                  | otherwise = n `mod` 10 : digits' (n `div` 10)

isSorted xs = all id $ zipWith (<=) xs (tail xs)

passwordCriteria n =
  isSorted xs && hasTwoGroup xs
  where xs = digits n
        hasTwoGroup xs = any id $ map ((>= 2) . length) $ group xs

enhancedPasswordCriteria n =
  isSorted xs && hasExactlyTwoGroup xs
  where xs = digits n
        hasExactlyTwoGroup xs = any id $ map ((== 2) . length) $ group xs
