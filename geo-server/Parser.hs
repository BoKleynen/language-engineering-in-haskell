{-# LANGUAGE BlockArguments #-}

module Parser (parseRegion, parseRegions) where

import Control.Applicative ( Alternative(empty) )
import Numeric (readFloat, readSigned)
import Text.Parsec

import GeoServerDeep


type Parser a = Parsec String () a

parseRegions :: String -> Either ParseError [Region]
parseRegions = parse pRegions ""

pRegions :: Parser [Region]
pRegions = many pRegion

parseRegion :: String -> Either ParseError Region
parseRegion = parse pRegion ""

pRegion :: Parser Region
pRegion = spaces *> pIntersect

pIntersect :: Parser Region
pIntersect = chainl1 pTranslate op
  where
    op = Intersection <$ symbol "/\\"

pTranslate :: Parser Region
pTranslate = do
    r <- pOutside
    rest r <|> return r
  where
    rest r = do
      symbol "->"
      d <- pDirection
      return (Translate d r)

pOutside :: Parser Region
pOutside = Outside <$> (symbol "!" *> pRegionTerm)
        <|> pRegionTerm

pRegionTerm :: Parser Region
pRegionTerm = between (symbol "|") (symbol "|") pIntersect
           <|> pRegionLit

pRegionLit :: Parser Region
pRegionLit = pCircle <|> pSquare

pCircle :: Parser Region
pCircle = Circle <$> between (char '(') (symbol ")") pNumber

pSquare :: Parser Region
pSquare = Square <$> between (char '[') (symbol "]") pNumber

pDirection :: Parser Direction
pDirection = between (symbol "(") (symbol ")") do
  dx <- lexeme pNumber
  symbol ","
  dy <- lexeme pNumber
  return (dx, dy)

pNumber :: Parser Double
pNumber = do
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

symbol :: String -> Parser String
symbol = lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces
