{-# LANGUAGE OverloadedStrings #-}
module Algo.FDTools.Parse
  ( parseGraph
  ) where

import Algo.FDTools.Types
import Algo.Common.Parse
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, spaceChar)
import Data.Void
import Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.Set as S

comment :: Parser ()
comment = try (char '#' *> anyChar `manyTill` optionalEol *> pure ())

graph :: Parser Graph
graph = S.fromList <$> (
  space *> skipp *>
  some (space *> edge <* space <* (comment <|> optionalEol) <* skipp) <* eof
  )
  where skipp = skipMany (try (space *> (comment <|> void eol)))

edge :: Parser Edge
edge = do
  p <- option "" $ try ((<>".") <$> ident <* char ':' <* space)
  try $ do
    l <- vertexList p
    space >> (string "->" <|> string "→" <|> string "\\to") >> space
    r <- vertexList p
    return (l, r)

vertexList :: T.Text -> Parser VertexList
vertexList p = S.fromList <$>
      (optional (try (char '(')) *> vertex p `sepBy` char ',' <* optional (try (char ')')))


vertex :: T.Text -> Parser Vertex
vertex p = Vertex . (p<>) <$> ident

parseGraph :: T.Text -> Either (ParseError Char Void) Graph
parseGraph = parse graph "input"
