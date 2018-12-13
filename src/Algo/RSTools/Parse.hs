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
import Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.HashSet as S

comment :: Parser ()
comment = try (char '#' *> anyChar `manyTill` optionalEol *> pure ())

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
  isKey <- maybe False (const True) <$> (optional $ char '*' <* space)
  Attribute isKey <$> (vertex Nothing <* space)
                      <*> (char ':' *> space *> domain)

domain :: Parser Domain
domain = string' "строка" *> pure DomainString
     <|> string' "текст" *> pure DomainText
     <|> string' "дата" *> pure DomainDateTime
     <|> string' "время" *> pure DomainDate
     <|> string' "дата/время" *> pure DomainTime
     <|> string' "целое" *> pure (DomainNumber Whole)
     <|> string' "натуральное" *> pure (DomainNumber Natural)
     <|> string' "дробное" *> pure (DomainNumber Rational)
     <|> string' "перечисление" *> space *> char '(' *>
          (DomainEnum . S.fromList <$>
            (space *> enumItem <* space) `sepBy` char ',') <* char ')'
     <|> (DomainOther <$> enumItem)

enumItem :: Parser T.Text
enumItem = T.pack <$> (many (noneOf (",()" :: [Char])))

parseRelations :: T.Text -> Either (ParseError Char Void) Relations
parseRelations = parse relations "input"
