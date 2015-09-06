{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module World.Parser where

import World.Field.Field
import Text.Trifecta.Parser
import Text.Trifecta.Combinators
import Text.Trifecta
import Text.Parser.Char
import Control.Applicative

import World.Data hiding (fst, snd)
import World.Field.Entities

parseFieldData src = parseFromFile prsAllMaps src

prsAllMaps = many prsOneMap

prsOneMap = do
        bcell <- prsBCell
        result <- braces $ many (prsElements $ BCell bcell)
        return result

prsBCell = char '@' >> prsCell

prsCSNum :: (Monad m, TokenParsing m) => m Cell
prsCSNum =  do
    i <- integer 
    comma
    j <- integer
    return $ Cell((fromInteger i):!:(fromInteger j))

prsCell = parens prsCSNum

prsCellSpan = parens $ do
                  start <- prsCSNum
                  colon
                  end <- prsCSNum
                  return (start, end)

prsElem = brackets $ token $ do 
    tipType <- integer
    spaces
    block <- char 'f' <|> char 't'
    return $ CellProps (block == 't') (toEnum $ fromInteger tipType)

prsElements bc = do
         spn <- prsCellSpan
         elm <- prsElem
         return $ tipList [MapCell bc c | c <- [(ACell $ fst spn) .. (ACell $ snd spn)]] elm


