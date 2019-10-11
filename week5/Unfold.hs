-- David Bergevoet	s1043736
-- Harm van der Heide s1047460
module Unfold
where

import Prelude hiding (take)
import qualified Data.List as L

unfoldr :: (t -> Maybe (a, t)) -> t -> [a]
unfoldr rep seed = produce seed
  where
    produce seed = case rep seed of 
       Just (a, new_seed) -> a : produce new_seed
       Nothing            -> []

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo rep seed = produce seed
  where
    produce seed = case rep seed of 
       Left l     -> l
       Right(a,ns) -> a : produce ns

take :: Int -> [a] -> [a]
take i x = unfoldr(\y -> if y == i then Nothing else Just(x!!y,y+1)) 0

aFilter :: (Ord a) =>(a -> Bool) -> [a] -> [a]
aFilter f x = let z = L.sort x in 
 unfoldr(\y -> if y >= length z || (f (z!!y)) == False then Nothing else Just(z!!y,y+1)) 0

fibs :: [Integer]
fibs = unfoldr(\(x,y) -> Just(x, (y,x+y))) (0,1)

primes :: [Integer]
primes = filter (>1) (unfoldr(\x -> if or( map(\y -> x `mod` y == 0) [2..x-1] ) then Just(0,x+1) else Just(x,x+1)) 2)


plusplus :: [a] -> [a] -> [a]
plusplus x y = apo (\z -> if z >= length x then Left(y) else Right(x!!z,z+1)) 0

aInsert :: (Ord a) => a -> [a] -> [a]
aInsert x [] = [x]
aInsert v x = apo (\i -> if v <= head i then Left(v:i) else if length i == 1 then Left([head i,v]) else Right(head i, tail i)) x
