-- David Bergevoet		1043736
-- Harm van der Heide	1047460

module Database
where

type Person  =  (Name, Age, FavouriteCourse)

type Name             =  String
type Age              =  Integer
type FavouriteCourse  =  String

frits, peter, ralf, david :: Person
frits  =  ("Frits",  33,  "Algorithms and Data Structures")
peter  =  ("Peter",  57,  "Imperative Programming")
ralf   =  ("Ralf",   33,  "Functional Programming")
david  =  ("David", 21, "Functional Programming")

students   ::  [Person]
students   =  [frits, peter, ralf, david]

age :: Person -> Age
age (_n, a, _c)  =  a

name             :: Person -> Name
name (n, _a, _c) = n

favouriteCourse  :: Person -> FavouriteCourse
favouriteCourse (_n, _a, c) = c

showPerson       :: Person -> String
showPerson (n, a, c) = "Name: '" ++ n ++ "' Age: "++ (show a) ++ " Favorite course: " ++ c

twins            :: Person -> Person -> Bool
twins (n, a, c) (n2, a2, c2)
 | a == a2 = True
 | otherwise = False
 

increaseAge      :: Person -> Person
increaseAge	(n, a, c) = (n, a+1, c)
