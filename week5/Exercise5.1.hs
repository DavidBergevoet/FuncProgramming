-- David Bergevoet	s1043736
-- Harm van der Heide s1047460
module Exercise51
where

allTrue :: [Bool] -> Bool
allTrue x = foldr (&&) True x


allFalse :: [Bool] -> Bool
allFalse x = not (foldr (||) False x)

member :: (Eq a) => a -> [a] -> Bool
member x y = foldr (\z b -> (z == x) || b) False y


smallest :: [Int] -> Int
smallest (x:xs) = foldr (\z b -> if z < b then z else b) x xs
 
largest :: [Int] -> Int
largest (x:xs) = foldr (\z b -> if z > b then z else b) x xs
