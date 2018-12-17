{-# LANGUAGE OverloadedStrings #-}
module Algo.ERTools.Parse
  ( parseER
  ) where

import Algo.ERTools.Types
import Algo.Common.Parse
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, spaceChar)
import Data.Either
import Data.Functor
import qualified Data.HashMap.Strict as M
import Data.Void
import Data.Text.Lazy (Text)
import Control.Monad
import Data.Maybe

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
  space
  paren <- optional $ char '('
  name <- ident
  when (isJust paren) $ void $ char ')'
  space
  optionalEol
  attrs <- many (attr $ AttrParentEnt name)
  return $ Entity name attrs $ case paren of
    Just _ -> Weak
    Nothing -> Strong

rel :: Parser Rel
rel = do
  _ <- char '='
  name <- ident
  conns <- newconnstx <|> oldconnstx <|> listconnstx
  optionalEol
  attrs <- many (attr $ AttrParentRel name)
  return $ Rel name conns attrs
  where
    oneOrMany = (char '1' $> One) <|> (oneOf' "MМ*" $> Many)
    oldconnstx =
      try $ some $ try $ do
        space
        _ <- char ':'
        space
        ct <- oneOrMany
        _ <- some spaceChar
        ent <- ident
        return (ct, ent)
    newconnstx =
      try $ some $ try $ do
        space
        ct <- (string "->" $> One) <|> (string "--" $> Many)
        space
        ent <- ident
        return (ct, ent)
    listconnstx =
      try $ do
        ents <- char '(' *> space *> sepBy1 ident colonSep <* space <* char ')'
        space
        cts <- char '(' *> space *> sepBy1 oneOrMany colonSep <* space <* char ')'
        space
        return $ zip (cts ++ repeat Many) ents
    colonSep = space *> char ':' <* space

attr :: AttrParent -> Parser Attr
attr par = Attr par <$> ident <*> option False (char '*' $> True) <* optionalEol

parseER :: Text -> Either (ParseError Char Void) ER
parseER = parse er "input"
