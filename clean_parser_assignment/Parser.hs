--module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,               lit, number, iter, accept, require, token,
--              spaces, word, (-#), (#-)) where

module Parser (module CoreParser, T, digit, digitVal, chars, letter, err,               lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

-- (a, [b, c, d])
-- vänstra sidan av !
-- char "abc" = Just ('a', "bc")
-- (char "bc") = Just ('b', "c")
-- char "c" = Just ('c', "")
-- char "" = Nothing

-- Just ('c', "") # Just ([], "") → Just (('c', []), "")
-- >-> cons → Just ("c", "")
-- Just ('b', "c") # Just ("c", "") → Just (('b', "c"), "")
-- >-> cons → Just ("bc", "")
-- Just ('a', "bc") # Just ("bc", "") → Just (('a', "bc"), "")
-- >-> cons → Just ("abc", "")

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = (m # n) >-> snd
-- >-> applies a function on a parser and returns a parser
-- snd takes the second element in a tuple, snd (x, y) = y

(#-) :: Parser a -> Parser b -> Parser a
m #- n = (m # n) >-> fst
-- fst takes the first element in a tuple, fst (x, y) = x


spaces :: Parser String
spaces =  iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char --(String -> Maybe(Char, String))
letter =  char ? isAlpha

-- letter "abc"
-- görs i char dvs char "abc": 
-- Just ('a', "bc")
-- görs i ?:
--    m cs = Just('a', "bc")
--    r = 'a', s = "bc"
--    p = isAlpha → isAlpha 'a' = True
-- Just ('a', "bc")


word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons
--
--chars 3 "abcde"     -- Just ("abc", "de")
--chars 3 "ab"        -- Nothing (bara 2 tecken)
--chars 0 "abc"       -- Just ("", "abc")


accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- chars 4 "read x;" = Just ("read", " x;")

require :: String -> Parser String
require w  = accept w ! err ("Expected \"" ++w++ "\"") 
-- testar parser accept w, om det inte funkar: err

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')
