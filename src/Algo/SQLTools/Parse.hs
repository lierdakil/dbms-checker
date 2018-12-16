{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}
module Algo.SQLTools.Parse
  ( parseTables
  ) where

import Algo.SQLTools.Types
import Algo.Common.Parse hiding (space)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (spaceChar)
import Data.Void
import Control.Monad
import qualified Data.Text.Lazy as T

tables :: Parser [Table]
tables = table `endBy1` (space *> char ';' <* space) <* space <* eof

table :: Parser Table
table = do
  void $ string' "create" *> space *> string' "table" *> space
  id' <- sqlIdent
  space
  void $ char '('
  space
  try (onlyCols id') <|> colsAndAttrs id'
  where
  onlyCols id' = do
    cols <- try column `sepBy1` (space *> char ',' <* space)
    space
    void $ char ')'
    return $ Table id' cols []
  colsAndAttrs id' = do
    cols <- try column `endBy1` (space *> char ',' <* space)
    space
    attrs <- tableAttr `sepBy1` (space *> char ',' <* space)
    space
    void $ char ')'
    return $ Table id' cols attrs

sqlIdent :: Parser T.Text
sqlIdent = char '"' *> ident <* char '"'
       <|> identNoSpace

column :: Parser Column
column = do
  id' <- sqlIdent
  space
  typ <- datatype
  space
  attrs <- colAttr `sepEndBy` space
  return $ Column id' typ attrs

datatype :: Parser DataType
datatype = typeChar
       <|> typeVarChar
       <|> typeText
       <|> typeInt
       <|> typeRational
       <|> typeFloat
       <|> typeDate
       <|> typeTime
       <|> typeDateTime
       <|> typeEnum
  where
  parseWithLen str ctr def = do
    void $ string' str
    space
    len <- maybe def read <$> optional (char '(' *> some digitChar <* char ')')
    return $ ctr len
  typeChar = parseWithLen "char" TypeChar 255
  typeVarChar = parseWithLen "varchar" TypeVarChar 65535
  typeText = parseWithLen "tinytext" TypeText 255
         <|> parseWithLen "text" TypeText 65535
         <|> parseWithLen "mediumtext" TypeText 16777215
         <|> parseWithLen "longtext" TypeText 4294967295
  typeInt = do
    len <- string' "tinyint" *> pure 1
       <|> string' "smallint" *> pure 2
       <|> string' "mediumint" *> pure 3
       <|> string' "int" *> pure 4
       <|> string' "bigint" *> pure 8
    space
    width <- fmap (Width . read) <$> optional (char '(' *> some digitChar <* char ')')
    space
    attrs <- numberAttr `sepEndBy` space
    return $ TypeInt len $ maybe id (:) width $ attrs
  typeRational = do
    void $ (string' "decimal" <|> string' "numeric")
    char '(' *> space *> (
      TypeRational <$> (read <$> some digitChar <* space)
                   <*> (char ',' *> space *> (read <$> some digitChar))
      ) <* space <* char ')'
      <*> numberAttr `sepEndBy` space
  typeFloat = do
    len <- string' "float" *> pure 4
       <|> string' "double" *> pure 8
    TypeFloat len <$> numberAttr `sepEndBy` space
  numberAttr = string' "unsigned" *> pure Unsigned
           <|> string' "zerofill" *> pure ZeroFill
           <|> string' "auto_increment" *> pure AutoIncrement
  typeDate = string' "date" *> space *> pure TypeDate
  typeTime = string' "time" *> space *> pure TypeTime
  typeDateTime = string' "datetime" *> space *> pure TypeDateTime
  typeEnum = do
    void $ string' "enum"
    space
    void $ char '('
    space
    vals <- strLit `sepBy1` (space *> char ',' <* space)
    space
    void $ char ')'
    return $ TypeEnum vals
  strLit :: Parser T.Text
  strLit = T.pack <$> (char '\'' *> manyTill printChar (char '\''))

colAttr :: Parser ColumnAttr
colAttr = string' "not null" *> pure NotNull
      <|> string' "primary key" *> pure PrimaryKey

tableAttr :: Parser TableAttr
tableAttr = tablePrimaryKey <|> tableForeignKey
  where
  tablePrimaryKey = TablePrimaryKey <$> (
    string' "primary key" *> space *> char '(' *> space *> (
      sqlIdent `sepBy1` (space *> char ',' <* space)
      ) <* space <* char ')')
  tableForeignKey = do
    ctr <- TableForeignKey <$> (
          string' "foreign key" *> space *> char '(' *> space *>
          sqlIdent `sepBy1` (space *> char ',' <* space)
          <* space <* char ')' <* space <* string' "references")
    refid <- sqlIdent
    space
    refcol <- char '(' *> space
              *> sqlIdent `sepBy1` (space *> char ',' <* space)
              <* space <* char ')'
    space
    uncurry (ctr refid refcol) <$> updateDelete
  fkAction = string' "no action" *> pure NoAction
         <|> string' "set null" *> pure ActionSetNull
         <|> string' "set default" *> pure ActionSetDefault
         <|> string' "restrict" *> pure ActionRestrict
         <|> string' "cascade" *> pure ActionCascade
  updateDelete = try firstUpdateThenDelete <|> firstDeleteThenUpdate
  firstDeleteThenUpdate = (,)
    <$> (string' "on delete" *> space *> fkAction <* space)
    <*> (string' "on update" *> space *> fkAction)
  firstUpdateThenDelete = (,)
    <$> (string' "on update" *> space *> fkAction <* space)
    <*> (string' "on delete" *> space *> fkAction)

parseTables :: T.Text -> Either (ParseError Char Void) [Table]
parseTables = parse tables "input"