{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
module Parser where

import Text.Parsec
import Numeric (readFloat, readSigned)
import Control.Applicative ( Alternative(empty) )
import Text.ParserCombinators.Parsec hiding (runParser, try)
import Text.Parsec.Expr

import GeoServerDeep


parseRegions :: String -> Either ParseError [Region]
parseRegions = parse pRegions ""

pRegions :: CharParser st [Region]
pRegions = sepEndBy pRegion newline

parseRegion :: String -> Either ParseError Region
parseRegion = parse pRegion ""

pRegion :: CharParser st Region
pRegion = buildExpressionParser table pRegionTerm
  where
    table = [ [ prefix "!" outside]
            ]

    prefix  name fun = Prefix do{ string name; return fun }

pRegionTerm :: CharParser st Region
pRegionTerm = between (char '|') (char '|') pRegion
           <|> pRegionLit

pRegionLit :: CharParser st Region
pRegionLit = pCircle <|> pSquare

pCircle :: CharParser st Region
pCircle = circle <$> between (char '(') (char ')') pNumber

pSquare :: CharParser st Region
pSquare = square <$> between (char '[') (char ']') pNumber

pNumber :: CharParser st Double
pNumber = do
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty
