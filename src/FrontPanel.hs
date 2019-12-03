module FrontPanel where

import Prelude hiding (Left, Right)
import Control.Monad.State
import qualified Data.Map.Strict as Map

data Wire = Up Int | Down Int | Left Int | Right Int deriving (Show)

parseWire :: String -> Wire
parseWire ('U':n) = Up    $ read n
parseWire ('D':n) = Down  $ read n
parseWire ('L':n) = Left  $ read n
parseWire ('R':n) = Right $ read n

type WireState = State (Int,Int,Int)

-- For a given bit of wire, return a list of points along its path and update
-- the current position and distance
plot :: Wire -> WireState [((Int,Int),Int)]
plot (Up n)    = state $ \(x,y,d) -> ([ ((x, y + i), d + i) | i <- [1..n] ], (x, y + n, d + n))
plot (Down n)  = state $ \(x,y,d) -> ([ ((x, y - i), d + i) | i <- [1..n] ], (x, y - n, d + n))
plot (Left n)  = state $ \(x,y,d) -> ([ ((x - i, y), d + i) | i <- [1..n] ], (x - n, y, d + n))
plot (Right n) = state $ \(x,y,d) -> ([ ((x + i, y), d + i) | i <- [1..n] ], (x + n, y, d + n))

-- Plots a list of wire bits, starting at the origin (0,0)
plotAll :: [Wire] -> Map.Map (Int,Int) Int
plotAll wires =
  let moves = map plot wires in
    Map.fromList $ concat $ evalState (sequence moves) (0,0,0)

manhattanDistance :: (Int,Int) -> Int
manhattanDistance (x,y) = (abs x) + (abs y)
