-- David Bergevoet 		1043736
-- Harm van der Heide	1047460
triangle :: Int -> Int -> String
line :: Int -> Int -> Int -> String

line n mx off = concat(map(\x -> " ")[1..(((mx-n) `div` 2)+off)]) ++  concat(map(\y -> "*")[1..n]) ++ "\n"

triangle n off= concat(map(\x -> line x n off) (filter(\y -> y `mod` 2 == 1) [1..n]))


christmasTree :: Int -> String
												 -- The offset is the last odd number - y devided by 2				This is done for a number of odd numbers
christmasTree n =  concat(map(\y -> triangle y (((last(take n (filter(\x -> x `mod` 2 == 1) [0..])))-y) `div` 2)) (take n (filter(\x -> x `mod` 2 == 1) [0..])))
