-- Harm van der Heide s1047460
-- David Bergevoet s1043736

module Mastermind
where
import System.Random
import Data.Char
import Data.List

dice :: Int ->IO Int
dice x = getStdRandom (randomR (1,x))

genCode :: Int -> Int -> IO[Int]
genCode 0 _ = return []
genCode len cnt = do
  r <- dice(cnt)
  rs <- genCode (len-1) cnt
  return (r:rs)

breakCode :: Int -> IO[Int] -> IO [Char]
breakCode 0 code = return " Failed"
breakCode tries code = do
  putStr "Guess code!\n"
  putStr "Tries left: "
  print tries
  x <- getLine
  y <- return (strToIntArr x)
  z <- code
  putStr "Red Pins: \n\t"
  print (redPins y z)
  putStr "White Pins:\n\t"
  print (whitePins y z)
  putStr "\n--------\n"
  if (y == z)
    then
      return "Code correct"
    else
      breakCode (tries-1) (return z)


strToIntArr :: String -> [Int]
strToIntArr str =  map (\x -> read x :: Int) (words str)

whitePins :: [Int] -> [Int] -> Int
whitePins guess code = sum( map(\x -> if elem x code then 1 else 0) guess) - (redPins guess code)


redPins :: [Int] -> [Int] -> Int
redPins [] _ = 0
redPins (x:xs) (y:ys)
  | x == y = 1 + (redPins xs ys)
  | otherwise = redPins xs ys
