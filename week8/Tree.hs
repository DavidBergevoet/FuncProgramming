-- Harm van der Heide s1047460
-- David Bergevoet s1043736

module Tree
where

-- Leaf trees.

data Tree elem  =  Leaf elem | Tree elem :^: Tree elem
  deriving (Show, Eq, Ord)
