module Parse where

import qualified System.Environment as Env

defaultMain :: FilePath -> (Int -> FilePath -> IO ()) -> IO ()
defaultMain defaultFileName f = do
  args <- Env.getArgs
  case args of
    [n,fileName] -> f (read n) fileName
    [n] -> f (read n) defaultFileName
    _ -> do
      putStrLn "Task 1:"
      f 1 defaultFileName
      putStrLn "Task 2:"
      f 2 defaultFileName

delimited :: (Char -> Bool) -> String -> [String]
delimited pred s = case dropWhile pred s of
                     "" -> []
                     s' -> w : delimited pred s''
                       where (w, s'') = break pred s'

charDelimited :: Char -> String -> [String]
charDelimited c s = delimited (\x -> x == c) s

commaDelimited :: String -> [String]
commaDelimited s = charDelimited ',' s
