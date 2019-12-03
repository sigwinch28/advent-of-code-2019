module FrontPanel where

import Prelude hiding (Left, Right)

import Control.Monad.State

data Wire = Up Int | Down Int | Left Int | Right Int deriving (Show)

parseWire :: String -> Wire
parseWire ('U':n) = Up    $ read n
parseWire ('D':n) = Down  $ read n
parseWire ('L':n) = Left  $ read n
parseWire ('R':n) = Right $ read n

type WireState = State (Int,Int)

data Orientation = Vertical | Horizontal
type Plot = (Orientation,(Int,Int),(Int,Int))

move :: Wire -> WireState ()
move (Up n)    = modify $ \(x,y) -> (x,   y+n)
move (Down n)  = modify $ \(x,y) -> (x,   y-n)
move (Left n)  = modify $ \(x,y) -> (x-n, y  )
move (Right n) = modify $ \(x,y) -> (x+n, y  )

orientation :: Wire -> Orientation
orientation (Up _)    = Vertical
orientation (Down _)  = Vertical
orientation (Left _)  = Horizontal
orientation (Right _) = Horizontal

plot :: Wire -> WireState Plot
plot wire = do
  start <- get
  move wire
  end <- get
  return (start, end)

plotAll :: [Wire] -> [Plot]
plotAll wires =
  let plots = sequence $ map plot wires in
    evalState plots (0,0)

