-- David Bergevoet s1043736
-- Harm van der Heide s1047460

module QuickTest (Probes, Property, (?->), (?=>))
where
import Data.List (sort,(\\) )

type Probes a    =  [a]

type Property a  =  a -> Bool

infixr 1  ?->, ?=>

(?->)   :: Probes a -> Property b -> Property (a -> b)
(?=>)   :: Probes a -> (a -> Property b) -> Property (a -> b)

probes ?-> prop  =  \ f -> and [ prop (f x)   | x <- probes ]
probes ?=> prop  =  \ f -> and [ prop x (f x) | x <- probes ]

ordered      :: (Ord a) => Property [a]
ordered [] = True
ordered [x] = True
-- The last part will make tuples of an element and its succesor
ordered xs = and( map( \(x,y) -> x <= y) (zip xs $ tail xs))



permutations :: (Eq a) => [a] -> Probes [a]
permutations [] = []
permutations [x] = [[x]]
permutations xs = [ x:ps | x <- xs , ps <- permutations ( xs\\[x] )]

-- A helper function for the runs function
helper :: (Ord a) => [a] -> [a]
helper [] = []
helper [x] = [x]
helper (x:xs) 
 | x <= head xs = [x] ++ helper xs
 | x >  head xs = [x]

runs:: (Ord a)=>[a]->[[a]]
runs [] = [[]]
runs [x] = [[x]]
runs [x,y] 
 | length(helper [x,y]) < 2 = [[x],[y]]
 | otherwise = [helper [x,y]]

runs (x:xs) 
 | ordered (x:xs) = [(x:xs)]
 | otherwise = [helper (x:xs)] ++ runs (drop (length( helper(x:xs))) (x:xs))


-- This is the testing procedure for the runs function. 
runsTest :: (Ord a) => [[a]] -> Bool
runsTest z = (z ?-> (\x -> and (map(\y -> ordered y) x))) (runs)


isqrt :: Integer -> Integer
isqrt n = loop 0 3 1
  where loop i k s  | s <= n      = loop (i + 1) (k + 2) (s + k)
                    | otherwise  = i
                    
isqrtTest :: [Integer] -> Bool
isqrtTest x = ( (map (\y -> y*y) x) ?=> (\a b -> a == b*b)) (isqrt)

infixr 4  <#>
(<#>) :: Probes a -> Probes b -> Probes (a, b)
probes <#> aProbe = zip (concat (replicate (length aProbe) probes)) (concat( map( replicate (length probes)) aProbe))
 
niftySort :: [a] -> [a]
niftySort _xs  =  []

trustedSort :: (Ord a) => [a] -> [a]
trustedSort  =  sort
