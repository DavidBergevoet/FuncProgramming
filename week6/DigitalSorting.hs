-- Harm van der Heide s1047460
-- David Bergevoet s1043736
module DigitalSorting
where
import Data.List (groupBy, sortBy)
import Data.Either

-- This needs to be implemented. This is due to the fromLeft' and fromRight' not being recognised
fromLeft' :: Either a b -> a
fromLeft' (Left a) = a

fromRight' :: Either a b -> b
fromRight' (Right b) = b


-- This is the implementation of the assoc function needed for the rank function of a tuple of keys
assoc :: ((a,b),c) -> (a,(b,c))
assoc ((x,y),z) = (x,(y,z))

-- Class implementation of Rank
class Rank key where
  sort  ::  [(key, val)] -> [val]
  rank  ::  [(key, val)] -> [[val]]
  sort  =  concat . rank
  
-- The generic functions of rank and sort
genericRank :: Ord key => [(key,val)] -> [[val]]
genericRank kvs = map (map (\x -> snd x)) (groupBy (\x y -> (fst x)==(fst y)) (sortBy (\x y -> compare (fst x) (fst y)) kvs))

genericSort :: (Ord key) => [(key, val)] -> [val]
genericSort kvs  = map snd (sortBy (\ kv1 kv2 -> compare (fst kv1) (fst kv2)) kvs)

-- The compare functions with sort and rank 
sortCompare :: (Ord key, Ord val) =>(key,val) -> (key,val) -> Ordering
sortCompare x y
 | head (genericSort [x,y]) == snd x && head (genericSort [y,x]) == snd y = EQ
 | head (genericSort [x,y]) == snd x = LT
 | otherwise = GT
 
rankCompare :: (Ord key, Ord val) => (key,val) -> (key,val) -> Ordering
rankCompare x y
 | length (genericRank [x,y]) == 1 = EQ
 | head (head (genericRank [x,y])) == snd x = LT
 | otherwise = GT
 
-- An example instance of Rank to test the functions with
instance Rank Char where
 rank x = genericRank x
 
 -- Rank implementation of the Either type
instance (Rank key1, Rank key2) => Rank (Either key1 key2) where
 rank x = let ls = [(k,v) | (k,v) <- x, isLeft k]
  in let rs = [(k,v) | (k,v) <- x, isRight k]
  in rank ( map (\(x,y) -> ((fromLeft' x),y)) ls) ++ rank ( map(\(x,y) -> ((fromRight' x),y)) rs)

-- Rank implementation of the tuple of keys
instance (Rank key1, Rank key2) => Rank (key1, key2) where
 rank xs =  concat (map rank (rank (map assoc xs)))

-- The implementation of the rank with an empty tuple
instance Rank () where
  sort kvs   =  map snd kvs
  rank kvs   =  [ map snd kvs | not (null kvs) ]

-- List and toList functions for converting an array into a List
type List elem  =  Either () (elem, [elem])

toList :: [elem] -> List elem
toList []        =  Left ()
toList (a : as)  =  Right (a, as)

-- The implementation of Rank with a list of keys
instance (Rank key) => Rank [key] where
 rank ksvs = rank [(toList ks, v) | (ks, v) <- ksvs]
 
-- repeatedSegments :: (Rank key) => Int -> [key] -> [[Integer]]

-- instance Rank Base where
