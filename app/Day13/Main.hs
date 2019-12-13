import qualified Intcode
import qualified ArcadeCabinet

import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map

import qualified Parse

defaultFileName = "data/day13.txt"

main :: IO ()
main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> ((map read) . Parse.commaDelimited)

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (print . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne prog = Map.size $ Map.filter (== 2) $ ArcadeCabinet.parse $ Intcode.runProg [] prog

taskTwo (_:prog) = snd $ last $ playOut
  where arcadeOut = Intcode.runProg (map fst playOut) (2:prog)
        playOut = ArcadeCabinet.play $ ArcadeCabinet.parseList arcadeOut
