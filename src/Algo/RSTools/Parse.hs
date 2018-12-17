{-# LANGUAGE OverloadedStrings #-}
module Algo.RSTools.Parse
  ( parseRelations
  ) where

import Algo.RSTools.Types
import Algo.FDTools.Parse (vertex)
import Algo.Common.Parse
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, spaceChar)
import Data.Void
import Data.Functor
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.HashSet as S

comment :: Parser ()
comment = try (char '#' *> anyChar `manyTill` optionalEol $> ())

relations :: Parser Relations
relations = Relations . S.fromList <$> (
  space *> skipp *>
  some (space *> relation <* space <* optional comment <* skipp) <* eof
  )
  where skipp = skipMany (try (space *> (comment <|> void eol)))

relation :: Parser Relation
relation = Relation . S.fromList <$>
      (char '(' *> (space *> attribute <* space) `sepBy` char ',' <* char ')')

attribute :: Parser Attribute
attribute = do
  isKey <- isJust <$> optional (char '*' <* space)
  Attribute isKey <$> (vertex Nothing <* space)
                      <*> (char ':' *> space *> domain)

domain :: Parser Domain
domain = string' "строка" $> DomainString
     <|> string' "текст" $> DomainText
     <|> string' "дата" $> DomainDateTime
     <|> string' "время" $> DomainDate
     <|> string' "дата/время" $> DomainTime
     <|> string' "целое" $> DomainNumber Whole
     <|> string' "натуральное" $> DomainNumber Natural
     <|> string' "дробное" $> DomainNumber Rational
     <|> string' "перечисление" *> space *> char '(' *>
          (DomainEnum . S.fromList <$>
            (space *> enumItem <* space) `sepBy` char ',') <* char ')'
     <|> (DomainOther <$> enumItem)

enumItem :: Parser T.Text
enumItem = T.pack <$> many (noneOf (",()" :: String))

parseRelations :: T.Text -> Either (ParseError Char Void) Relations
parseRelations = parse relations "input"
