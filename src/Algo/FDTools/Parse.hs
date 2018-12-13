{-# LANGUAGE OverloadedStrings #-}
module Algo.FDTools.Parse
  ( parseGraph, vertex
  ) where

import Algo.FDTools.Types
import Algo.Common.Parse
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, spaceChar)
import Data.Void
import Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

comment :: Parser ()
comment = try (char '#' *> anyChar `manyTill` optionalEol *> pure ())

graph :: Parser Graph
graph = M.fromListWith (<>) <$> (
  space *> skipp *>
  some (space *> edge <* space <* (comment <|> optionalEol) <* skipp) <* eof
  )
  where skipp = skipMany (try (space *> (comment <|> void eol)))

edge :: Parser Edge
edge = do
  p <- optional $ try (ident <* char ':' <* space)
  try $ do
    l <- vertexList p
    space >> (string "->" <|> string "→" <|> string "\\to") >> space
    r <- vertexList p
    return (l, r)

vertexList :: Maybe T.Text -> Parser VertexList
vertexList p = S.fromList <$>
      (optional (try (char '(')) *> vertex p `sepBy` char ',' <* optional (try (char ')')))


vertex :: Maybe T.Text -> Parser Vertex
vertex (Just p) = Vertex p <$> ident
vertex Nothing = Vertex <$> (ident <* char '.') <*> (oneOf ("θΘ∅0" :: [Char]) *> pure "Θ" <|> ident)

parseGraph :: T.Text -> Either (ParseError Char Void) Graph
parseGraph = parse graph "input"
