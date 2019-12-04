module FuelDepot where

digits :: Int -> [Int]
digits n = reverse $ digits' n
  where digits' n | n < 10    = [n]
                  | otherwise = n `mod` 10 : digits' (n `div` 10)

hasTwoAdjacentDigits xs = any id $ zipWith (==) xs (tail xs)

hasExactlyTwoAdjacentDigits xs =
  hasAdjacency && not hasDoubleAdjacency
  where adjacencies = zipWith (==) xs (tail xs)
        hasAdjacency = any id $ adjacencies
        hasDoubleAdjacency = any id $ zipWith (&&) adjacencies (tail adjacencies)

isSorted xs = all id $ zipWith (<=) xs (tail xs)

passwordCriteria n =
  hasTwoAdjacentDigits xs && isSorted xs
  where xs = digits n

enhancedPasswordCriteria n =
  hasExactlyTwoAdjacentDigits xs && isSorted xs
  where xs = digits n
