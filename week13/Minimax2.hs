{-# LANGUAGE TypeFamilies #-}

module Minimax2
where
import Squiggol

data Tree elem  =  Node elem [Tree elem]

size, depth :: Tree elem -> Integer
size  = error "size not yet defined"
depth = error "depth not yet defined"

gametree :: (position -> [position]) -> (position -> Tree position)
gametree = error "gametree not yet defined"

winning  :: Tree position -> Bool
winning = error "winning not yet defined"