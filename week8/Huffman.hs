-- Harm van der Heide s1047460
-- David Bergevoet s1043736

module Huffman
where
import Satellite
import Tree

import Data.List

-- -------------------------------------------------------------------------------

-- Warm-up: constructing a frequency table.

frequencies  :: [Char] -> [With Int Char]
frequencies str = map (\x -> ((length x):-(head x))) (group (sort str))

-- -------------------------------------------------------------------------------

leafify :: [With Int Char] -> [With Int (Tree Char)]
leafify x = map (\(y:-z) -> (y:-Leaf z))  (sort x)

treefy :: [With Int (Tree Char)] -> [With Int (Tree Char)]
treefy (x:xs)
 | length (x:xs) == 1 = (x:xs)
 | otherwise = treefy (sort ((((sat_val x) + (sat_val (head xs))):-(satellite x :^: satellite (head xs))) : delete (head xs) xs))

-- Constructing a Huffman tree.
                              -- Tree char
huffman :: [With Int Char] -> Tree Char
huffman (x:xs) = satellite (head (treefy (leafify (x:xs))))

-- -------------------------------------------------------------------------------

-- Encoding ASCII text.

data Bit = O | I
  deriving (Show, Eq, Ord)

encode :: Tree Char -> [Char] -> [Bit]
encode tree str = concat (concat (map (\x -> (map (\(y,z) -> if x == y then z else []) (codes tree))) str))


treecode :: Tree Char -> (Char,[Bit]) -> [(Char,[Bit])]
treecode (Leaf c) (x,(y:ys)) = [(c,(y:ys))]
treecode (x :^: y) (c,z) = (treecode x (c,(z ++ [O]))) ++ (treecode y (c,(z ++ [I])))

codes :: Tree Char -> [(Char, [Bit])]
codes t = treecode t (' ',[])

-- -------------------------------------------------------------------------------

-- Decoding a Huffman binary.

groot :: Tree Char -> [Bit] -> (Char,Int)
groot (Leaf n) _ = (n,0)
groot (x :^: y) (z:zs)
 | z == O = ((fst (groot x zs)), ((snd (groot x zs)) +1))
 | z == I = ((fst (groot y zs)), ((snd (groot y zs)) +1))

decode :: Tree Char -> [Bit] -> [Char]
decode tree [] = []
decode tree (x:xs) = let (a,b) = (groot tree (x:xs)) in
  (a : decode tree (drop b (x:xs)))

-- -------------------------------------------------------------------------------

-- Some test data.

hw, why :: String
hw =
  "hello world"

-- code = huffman (frequencies hw)
-- encode code hw
-- decode code it
-- decode code it == hw

why =
  "As software becomes more and more complex, it\n\
  \is  more  and  more important to structure it\n\
  \well.  Well-structured  software  is  easy to\n\
  \write,   easy   to   debug,  and  provides  a\n\
  \collection  of modules that can be re-used to\n\
  \reduce future programming costs. Conventional\n\
  \languages place a conceptual limit on the way\n\
  \problems   can   be  modularised.  Functional\n\
  \languages  push  those  limits  back. In this\n\
  \paper we show that two features of functional\n\
  \languages    in    particular,   higher-order\n\
  \functions and lazy evaluation, can contribute\n\
  \greatly  to  modularity.  Since modularity is\n\
  \the key to successful programming, functional\n\
  \languages  are  vitally important to the real\n\
  \world."

-- code = huffman (frequencies why)
-- encode code why
-- decode code it
-- decode code it == why
