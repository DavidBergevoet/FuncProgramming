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
area (Circle r) = 2.0 * pi * ((read (show r) :: Double) * (read (show r) :: Double))
area (Square l) = (read (show l) :: Double) * (read (show l) :: Double)
area (Rectangle l w) = (read (show l) :: Double) * (read (show w) :: Double)


-- Perimeter function
perimeter :: Shape -> Double
perimeter (Circle r) = 2.0 * pi * (read (show r) :: Double)
perimeter (Square l) = 4.0 * (read (show l) :: Double)
perimeter (Rectangle l w) = (2.0 * (read (show l) :: Double)) + (2.0 * (read (show w) :: Double))


-- Center function
center :: Shape -> (Double, Double)
center (Circle r) = ((read (show r) :: Double),(read (show r) :: Double))
center (Square l) = (((read (show l) :: Double) / 2.0), ((read (show l) :: Double) / 2.0))
center (Rectangle l w) = (((read (show w) :: Double) / 2.0), ((read (show l) :: Double) / 2.0))


-- Bounding Box function
boundingBox :: Shape -> (Double, Double)
boundingBox (Circle r) = (((read (show r) :: Double)*2.0),((read (show r) :: Double)*2.0))
boundingBox (Square l) = ((read (show l) :: Double), (read (show l) :: Double))
boundingBox (Rectangle l w) = ((read (show w) :: Double), (read (show l) :: Double)) 

-- Variable collection
cir, squ, rect :: Shape
cir = Circle(1/3)
squ = Square(pi)
rect = Rectangle 2.0 4.0


main = print(boundingBox rect)

