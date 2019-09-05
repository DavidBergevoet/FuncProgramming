module Database (database)
where

-- Class person
type Name = String
type Age = Integer
type FavoriteCourse = String


age :: Person -> Age 
age (_n, a, _c) = a

-- Instances of person
frits, peter, ralf, david :: Person

frits = ("Frits", 33, "Algorithms")
peter = ("Peter", 57, "Imperative")
ralf = ("Ralf", 33, "Func")
david = ("David", 11, "Calculus")

students :: [Person]
students = [frits, peter, ralf, david]

-- Main
