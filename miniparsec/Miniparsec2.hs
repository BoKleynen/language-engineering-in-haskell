{-# LANGUAGE BlockArguments #-}

module Miniparsec2 where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char


type Parser = StateT String []

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

parser :: (String -> [(a, String)]) -> Parser a
parser = StateT

item :: Parser Char
item = do
  (c:cs) <- get
  put cs
  return c

-- All the functions that follow are implemented exactly the same
-- as in Miniparsec
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  guard (p x)
  return x

char :: Char -> Parser Char
char x = sat (x ==)

digit :: Parser Char
digit = sat \x -> '0' <= x && x <= '9'

lower :: Parser Char
lower = sat \x -> 'a' <= x && x <= 'z'

upper :: Parser Char
upper = sat \x -> 'A' <= x && x <= 'Z'

letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

word :: Parser String
word = many letter

string :: String -> Parser String
string = mapM char

ident :: Parser String
ident = liftM2 (:) lower (many alphanum)

nat :: Parser Int
nat = fmap (\x -> ord x - ord '0') digit `chainl1` pure op
  where
    m `op` n = 10 * m + n

int :: Parser Int
int = op <*> nat
  where
    op = char '-' >> pure negate <|> pure id

ints :: Parser [Int]
ints = bracket (char '[') (int `sepBy1` char ',') (char ']')

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = p `sepBy1` sep <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = liftM2 (:) p (many (sep >> p))

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = open *> p <* close

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v = chainl1 p op <|> pure v

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (op <*> pure x <*> p) <|> pure x

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v = chainr1 p op <|> pure v

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = do
  x <- p
  f <- op
  y <- chainr1 p op
  pure (f x y) <|> pure x

ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (<|>) [p >> pure op | (p,op) <- xs]

first :: Parser a -> Parser a
first p = parser \s -> case runParser p s of
  [] -> []
  (x:_) -> [x]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p <|> q)

number :: Parser Int
number = nat +++ pure 0

colour :: Parser String
colour = string "yellow" +++ string "orange"

spaces :: Parser ()
spaces = void $ some (sat isSpace)

comment :: Parser ()
comment = void $ string "--" >> many (sat (/= '\n'))

junk :: Parser ()
junk = void $ many (spaces +++ comment)

parse :: Parser a -> Parser a
parse p = junk >> p

token :: Parser a -> Parser a
token p = p <* junk

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: [String] -> Parser String
identifier ks = do
  x <- ident
  guard (x `notElem` ks)
  return x
