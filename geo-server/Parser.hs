{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Parser (parseRegion, parseRegions) where

import Text.Parsec
import Numeric (readFloat, readSigned)
import Control.Applicative ( Alternative(empty) )
import Text.ParserCombinators.Parsec hiding (runParser, try)

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
    op = (/\) <$ string "/\\"

pTranslate :: CharParser st Region
pTranslate = do
    r <- pOutside
    rest r <|> return r
  where
    rest r = do
      string "->"
      d <- pDirection
      return (Translate d r)

pOutside :: CharParser st Region
pOutside = outside <$> (char '!' *> pRegionTerm)
        <|> pRegionTerm

pRegionTerm :: CharParser st Region
pRegionTerm = between (char '|') (char '|') pIntersect
           <|> pRegionLit

pRegionLit :: CharParser st Region
pRegionLit = pCircle <|> pSquare

pCircle :: CharParser st Region
pCircle = circle <$> between (char '(') (char ')') pNumber

pSquare :: CharParser st Region
pSquare = square <$> between (char '[') (char ']') pNumber

pDirection :: CharParser st Direction
pDirection = do
  char '('
  dx <- pNumber
  char ','
  dy <- pNumber
  char ')'
  return (dx, dy)

pNumber :: CharParser st Double
pNumber = do
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty
