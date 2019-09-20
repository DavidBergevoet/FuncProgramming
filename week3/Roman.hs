-- David Bergevoet s1043736
-- Harm van der Heide s1047460

module Roman
where
import Data.List

data RD = M | D | C | L | X | V | I
instance Eq RD where
 M == M = True
 D == D = True
 C == C = True
 L == L = True
 X == X = True
 V == V = True
 I == I = True
 _ == _ = False
 
data Roman = Roman [RD]

rd2int :: RD -> Int
rd2int x
 | x == M = 1000
 | x == D = 500
 | x == C = 100
 | x == L = 50
 | x == X = 10
 | x == V = 5
 | x == I = 1
 
 
converter :: [Int] -> Int
converter [] = 0
converter [x] = x
converter [x,y]
 | x > y = x - y
 | otherwise = x + y
 
converter (x:xs)
 | x >= head xs = (converter xs) -x
 | x < head xs = (converter xs) + x 


roman2int :: Roman -> Int
roman2int (Roman (x:xs)) = converter ( map (\a-> sum a) ( groupBy (==) (reverse (map(\z-> rd2int z) (x:xs)))))


convList = [("M",1000),("CM",900),("D",500),("CD",400),("C",100),("XC",90),("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1)]
 

int2roman :: Int -> String
int2roman x
 | x <= 0 = "" 
 | x > 0 = fst(c) ++ int2roman ( x- snd(c))
 where c = head ( filter (\(y,z) -> (x-z) >= 0) convList)
