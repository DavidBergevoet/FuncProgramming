module Char
where
import Data.Char

equal      :: String -> String -> Bool
equal x y = map toLower x == map toLower y

isNumeral  :: String -> Bool
isNumeral x = and (map isNumber x)

isBlank    :: String -> Bool
isBlank x = and (map isSpace x)

fromDigit  :: Char -> Int
fromDigit x 
 | isNumber x = digitToInt x
 | otherwise = 0

toDigit    :: Int -> Char
toDigit x = intToDigit x

shift      :: Int -> Char -> Char
shift x y 
 | (ord y) + x > 90 = chr( ((ord y) + x -26))
 | otherwise = chr((ord y)+x)
 
 
msg  ::  String
msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
        \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"
        
decode :: Int -> String
decode x = map(\y -> if isSpace y then y else shift x y) msg

-- The msg string is encrypted with 7 so we have to do 26-7 to get the answer
fullDecode :: String
fullDecode = decode 19
