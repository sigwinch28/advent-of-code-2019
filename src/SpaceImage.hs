module SpaceImage where
import Data.List (mapAccumL)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Char (ord)

data Pixel = Black | White | Transparent deriving (Eq, Show)

type Layer = [[Pixel]]
type Image = [Layer]

instance Semigroup Pixel where
  Transparent <> p2 = p2
  p1          <> p2 = p1

instance Monoid Pixel where
  mempty = Transparent


parsePixel :: Char -> Pixel
parsePixel '0' = Black
parsePixel '1' = White
parsePixel '2' = Transparent

parseImage :: Int -> Int -> String -> Image
parseImage width height img = chunksOf height $ chunksOf width $ map parsePixel img

flattenImage :: [Layer] -> Layer
flattenImage layers = foldl (zipWith (zipWith mappend)) blank layers
  where blank = (replicate height (replicate width Transparent))
        height = length $ head layers
        width = length $ head $ head layers

showLayer :: Layer -> String
showLayer [] = ""
showLayer (row:rows) = map showPixel row ++ "\n" ++ showLayer rows

showPixel :: Pixel -> Char
showPixel Black = '.'
showPixel White = '#'
showPixel Transparent = ' '
