module NBody.Parse (parseBody) where

import NBody (Body, newBody)

import Control.Monad (liftM2)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

angles f = between (char '<') (char '>') f

unsignedNumber :: ReadP Int
unsignedNumber = fmap read $ munch isDigit

signedNumber :: ReadP Int
signedNumber = fmap read $ liftM2 (:) (char '-') (munch isDigit)

number = signedNumber <++ unsignedNumber

separator = do
  satisfy $ (== ',')
  skipSpaces

component name = do
  char name
  char '='
  number

components = do
  x <- component 'x'
  separator
  y <- component 'y'
  separator
  z <- component 'z'
  return (x, y, z)

body = angles components

readBody :: ReadS Body
readBody = readP_to_S $ fmap (\(x,y,z) -> newBody x y z) body

parseBody str = fst $ head $ readBody str
