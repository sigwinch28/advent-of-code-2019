module ArcadeCabinet where

import qualified Data.Map as Map


parse :: [Int] -> Map.Map (Int,Int) Int
parse xs = parse' xs Map.empty
  where parse' [] acc = acc
        parse' (x:y:v:xs) acc = parse' xs (Map.insert (x,y) v acc)


parseList :: [Int] -> [(Int,Int,Int)]
parseList [] = []
parseList (x:y:v:xs) = (x,y,v) : parseList xs


play :: [(Int,Int,Int)] -> [(Int,Int)]
play xs = play' xs 0 0 0 0 0
  where play' []     score ball paddle blocks joystick = [(joystick,score)]
        play' (d:xs) score ball paddle blocks joystick =
          case d of
            (-1,0,score') -> play' xs score' ball paddle blocks joystick
            (x,y,2) -> play' xs score ball paddle (blocks+1) joystick
            (paddle',y,3) -> play' xs score ball paddle' blocks joystick
            (ball',y,4) -> let joystick' = calcJoystick paddle ball' in (joystick',score) : play' xs score ball' paddle blocks joystick'
            _ -> play' xs score ball paddle blocks joystick
        calcJoystick paddle ball | paddle < ball  = 1
                                 | paddle == ball = 0
                                 | paddle > ball  = -1
