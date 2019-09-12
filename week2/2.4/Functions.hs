module Functions
where

swap :: (Int, Int) -> (Int, Int)
swap (x,y) = (y,x)

add :: (Int, Int) -> (Int, Int)
add (x,y) = (x+1,y+1)

sixtyNine2FourTwenty :: (Int, Int) -> (Int, Int)
sixtyNine2FourTwenty (x,y)
 | x == 69 && y == 69 = (420,420)
 | x == 69 = (420,y)
 | y == 69 = (x,420)
 | otherwise = (x,y)

convert2Single :: (Int,(Char, Bool)) -> (Int, Char, Bool)
convert2Single (x,(y,z)) = (x,y,z)

convert2Double :: (Int,Char,Bool) -> (Int,(Char,Bool))
convert2Double (x,y,z) = (x,(y,z))

