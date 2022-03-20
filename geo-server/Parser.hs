{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Parser (parseRegion, parseRegions) where

import Control.Applicative ( Alternative(empty) )
import Numeric (readFloat, readSigned)

import Text.Parsec
import Text.ParserCombinators.Parsec

import GeoServerDeep


parseRegions :: String -> Either ParseError [Region]
parseRegions = parse pRegions ""

pRegions :: CharParser st [Region]
pRegions = sepEndBy pIntersect newline

parseRegion :: String -> Either ParseError Region
parseRegion = parse pIntersect ""

pIntersect :: CharParser st Region
pIntersect = chainl1 pTranslate op
  where
    op = Intersection <$ symbol "/\\"

pTranslate :: CharParser st Region
pTranslate = do
    r <- pOutside
    rest r <|> return r
  where
    rest r = do
      symbol "->"
      d <- pDirection
      return (Translate d r)

pOutside :: CharParser st Region
pOutside = Outside <$> (symbol "!" *> pRegionTerm)
        <|> pRegionTerm

pRegionTerm :: CharParser st Region
pRegionTerm = between (symbol "|") (symbol "|") pIntersect
           <|> pRegionLit

pRegionLit :: CharParser st Region
pRegionLit = pCircle <|> pSquare

pCircle :: CharParser st Region
pCircle = circle <$> between (char '(') (symbol ")") pNumber

pSquare :: CharParser st Region
pSquare = square <$> between (char '[') (symbol "]") pNumber

pDirection :: CharParser st Direction
pDirection = do
  symbol "("
  dx <- pNumber
  spaces
  symbol ","
  dy <- pNumber
  spaces
  symbol ")"
  return (dx, dy)

pNumber :: CharParser st Double
pNumber = do
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

symbol :: String -> CharParser st String
symbol s = string s <* spaces
