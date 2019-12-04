module FuelDepot where

import Data.List

ascending :: Ord a => [a] -> Bool
ascending xs = and $ zipWith (<=) xs (tail xs)

passwordCriteria n =
  let digits = show n in
    ascending digits &&  any ((>= 2) . length) (group digits)

enhancedPasswordCriteria n =
  let digits = show n in
    ascending digits &&  any ((== 2) . length) (group digits)
