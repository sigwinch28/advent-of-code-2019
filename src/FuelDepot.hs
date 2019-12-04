module FuelDepot where

digits :: Int -> [Int]
digits n = reverse $ digits' n
  where digits' n | n < 10    = [n]
                  | otherwise = n `mod` 10 : digits' (n `div` 10)

runs :: Ord a => [a] -> [[a]]
runs [] = []
runs (x:xs) = runs' x xs [x]
  where
    runs' _ [] acc = [acc]
    runs' x (y:ys) acc =
      if x == y then
        runs' x ys (y:acc)
      else
        acc : (runs' y ys [y])

isSorted xs = all id $ zipWith (<=) xs (tail xs)

passwordCriteria n =
  isSorted xs && hasTwoGroup xs
  where xs = digits n
        hasTwoGroup xs = any id $ map ((>= 2) . length) $ runs xs

enhancedPasswordCriteria n =
  isSorted xs && hasExactlyTwoGroup xs
  where xs = digits n
        hasExactlyTwoGroup xs = any id $ map ((== 2) . length) $ runs xs
