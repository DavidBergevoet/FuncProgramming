-- Harm van der Heide s1047460
-- David Bergevoet s1043736

module Mastermind
where
import System.Random
import Data.Char
import Data.List

-- This is the base function which executes the mastermind functions and asks for the variables
mastermind :: IO()
mastermind = do
  putStr "Enter code length: "
  len <- getLine
  putStr "Enter code numbers 1.."
  nr <- getLine
  putStr "Enter number of tries: "
  tries <- getLine
  z <- (breakCode (read tries :: Int) (genCode (read len :: Int ) (read nr :: Int)))
  print z
  return ()

-- Generate a number between 1 and the given number
dice :: Int ->IO Int
dice x = getStdRandom (randomR (1,x))

-- This will generate a code with a given length and the maximal number in the code
genCode :: Int -> Int -> IO[Int]
genCode 0 _ = return []
genCode len cnt = do
  r <- dice(cnt)
  rs <- genCode (len-1) cnt
  return (r:rs)

-- This lets the user break a given code and returns Failed or Correct based on the tries and code
breakCode :: Int -> IO[Int] -> IO [Char]
breakCode 0 code = do
  z <- code
  print z
  return "Failed"
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

-- This will convert a string to an int array
strToIntArr :: String -> [Int]
strToIntArr str =  map (\x -> read x :: Int) (words str)

-- This will calculate the number of white pins with a given guess and code
whitePins :: [Int] -> [Int] -> Int
whitePins guess code =
  let innerCompare x y = if (count x code) < (count x guess) then (count x code) else (count x guess) in
    let compare x y = if x == y then (innerCompare x y) else 0 in
      let innerMap x = map (\y -> compare x y) (unique code) in
        sum (concat(map (\x-> innerMap x ) (unique guess))) - (redPins guess code)

-- This will make a list with only the unique elements
unique :: Eq a => [a] -> [a]
unique []       = []
unique (x : xs) = x : unique (filter (x /=) xs)

-- This will count the number of elements in a given list
count :: Ord a => a -> [a] -> Int
count x xs = (length . filter (== x)) xs

-- This will calculate the number of red pins based on a guess and code
redPins :: [Int] -> [Int] -> Int
redPins [] _ = 0
redPins (x:xs) (y:ys)
  | x == y = 1 + (redPins xs ys)
  | otherwise = redPins xs ys
