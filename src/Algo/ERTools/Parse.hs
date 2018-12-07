{-# LANGUAGE OverloadedStrings #-}
module Algo.ERTools.Parse
  ( parseER
  ) where

import Algo.ERTools.Types
import Algo.Common.Parse
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, spaceChar)
import Data.Either
import qualified Data.Map as M
import Data.Void
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

er :: Parser ER
er = do
  space
  (rels, ents) <- partitionEithers <$> many ((Right <$> entity) <|> (Left <$> rel))
  space
  eof
  let entMap = M.fromList $ map (\x -> (entName x, x)) ents
  return $ ER entMap rels

entity :: Parser Entity
entity = do
  _ <- char '*'
  name <- ident
  optionalEol
  attrs <- many attr
  return $ Entity name attrs

rel :: Parser Rel
rel = do
  _ <- char '='
  name <- ident
  conns <- newconnstx <|> oldconnstx <|> listconnstx
  optionalEol
  attrs <- many attr
  return $ Rel name conns attrs
  where
    oneOrMany = (char '1' *> pure One) <|> (oneOf' "MМ*" *> pure Many)
    strongOrWeak = char '(' *> space *> (Weak <$> oneOrMany) <* space <* char ')'
              <|> Strong <$> oneOrMany
    oldconnstx =
      try $ some $ try $ do
        space
        _ <- char ':'
        space
        ct <- strongOrWeak
        _ <- some spaceChar
        ent <- ident
        return (ct, ent)
    newconnstx =
      try $ some $ try $ do
        space
        ct <- Strong <$> ((string "->" *> pure One) <|> (string "--" *> pure Many))
           <|> Weak <$> ((string "=>" *> pure One) <|> (string "==" *> pure Many))
        space
        ent <- ident
        return (ct, ent)
    listconnstx =
      try $ do
        ents <- char '(' *> space *> sepBy1 ident colonSep <* space <* char ')'
        space
        cts <- char '(' *> space *> sepBy1 strongOrWeak colonSep <* space <* char ')'
        space
        return $ zip (cts ++ repeat (Strong Many)) ents
    colonSep = space *> char ':' <* space

attr :: Parser Attr
attr = Attr <$> ident <*> option False (char '*' *> pure True) <* optionalEol

parseER :: Text -> Either (ParseError Char Void) ER
parseER = parse er "input"
