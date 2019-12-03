module Parse where

delimited :: (Char -> Bool) -> String -> [String]
delimited pred s = case dropWhile pred s of
                     "" -> []
                     s' -> w : delimited pred s''
                       where (w, s'') = break pred s'

charDelimited :: Char -> String -> [String]
charDelimited c s = delimited (\x -> x == c) s

commaDelimited :: String -> [String]
commaDelimited s = charDelimited ',' s
