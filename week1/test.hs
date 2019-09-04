f3 :: String -> Int -> String
f3 s n
 | n <= 0 = ""
 | otherwise = s ++ f3 s (n-1)

main = print (f3 "test" 2)
