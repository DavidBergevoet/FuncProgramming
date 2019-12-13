{-# LANGUAGE LambdaCase  #-}

import Prelude hiding (foldr)

type LIST a = Maybe (a,[a])

out :: [a] -> LIST a
out []      = Nothing
out (x:xs)  = Just (x,xs)

inn :: LIST a -> [a]
inn Nothing       = []
inn (Just (x,xs)) = x:xs

foldr   :: (a -> b -> b) -> b -> ([a] -> b)
foldr f e = consume where
  consume []     = e
  consume (x:xs) = f x (consume xs)

foldRAsfoldr :: (Maybe (a,b) -> b) -> ([a] -> b)
foldRAsfoldr = error "foldRAsfoldr undefined"

foldrAsfoldR :: (a -> b -> b) -> b -> ([a] -> b)
foldrAsfoldR = error "foldrAsfoldR undefined"

foldR   :: (Maybe (a,b) -> b) -> ([a] -> b)
foldR al   = consume where
   consume = al . fmap (fmap consume) . out

foldrR :: (a -> b -> b) -> b -> ([a] -> b)


unfoldR :: (b -> Maybe (a, b)) -> (b -> [a])
unfoldR co = produce where
   produce = inn . fmap (fmap produce) . co

data LISTB elem res = NIL | CONS elem res

mbP2LB :: Maybe (elem,res) -> LISTB elem res
mbP2LB = error "mbP2LB undefined"

lB2MbP :: LISTB elem res -> Maybe(elem,res)
lB2MbP = error " undefined"

instance Functor (LISTB elem) where
    fmap = error "fmap undefined"

outB :: [a] -> LISTB a [a]
outB = error "outB undefined"
innB :: LISTB a [a] -> [a]
innB = error "innB undefined"

-- foldRB ::
foldRB al   = consume where
   consume = al . fmap consume . outB

-- unfoldRB ::
unfoldRB co = consume where
   consume = error "consume undefined"

emap :: (t -> elem) -> LISTB t res -> LISTB elem res
emap f NIL        = NIL
emap f (CONS a b) = CONS (f a) b

mapAsFold  = error "mapAsFold undefined"
mapAsunold = error "mapAsunold undefined"
