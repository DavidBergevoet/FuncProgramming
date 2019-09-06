-- David Bergevoet		1043736
module Shapes
where

data Shape
 = Circle Double --radius
 | Square Double --length
 | Rectangle Double Double --length and width
 deriving(Show)

-- ShowShape function
showShape :: Shape -> String
showShape (Circle r) = "circle of radius " ++ show r
showShape (Square l) = "square of length " ++ show l
showShape (Rectangle l w) = "rectangle of length " ++ show l ++ " and width " ++ show w

-- Area function
area :: Shape -> Double
area (Circle r) = 2.0 * pi * (r * r)
area (Square l) = l * l
area (Rectangle l w) = l * w


-- Perimeter function
perimeter :: Shape -> Double
perimeter (Circle r) = 2.0 * pi * r
perimeter (Square l) = 4.0 * l
perimeter (Rectangle l w) = (2.0 * l) + w


-- Center function
center :: Shape -> (Double, Double)
center (Circle r) = (r,r)
center (Square l) = ((l / 2.0), (l / 2.0))
center (Rectangle l w) = ((w / 2.0), (l / 2.0))


-- Bounding Box function
boundingBox :: Shape -> (Double, Double)
boundingBox (Circle r) = ((r*2.0),(r*2.0))
boundingBox (Square l) = (l, l)
boundingBox (Rectangle l w) = (w, l) 

-- Variable collection
cir, squ, rect :: Shape
cir = Circle(1/3)
squ = Square(pi)
rect = Rectangle 2.0 4.0
