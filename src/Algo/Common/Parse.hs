{-# LANGUAGE OverloadedStrings #-}
module Algo.Common.Parse where

import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, spaceChar)
import Data.Void
import Data.Functor
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

type Parser = Parsec Void Text
type ParseErr = ParseError Char Void

optionalEol :: Parser ()
optionalEol = try (some eol $> ()) <|> eof

oneOf' :: String -> Parser Char
oneOf' = oneOf

space :: Parser ()
space = skipMany spaceChar

spaceChar :: Parser Char
spaceChar = char ' '

ident :: Parser Text
ident = T.strip . T.pack <$> (space *> some (alphaNumChar <|> oneOf' " _№"))

identNoSpace :: Parser Text
identNoSpace = T.strip . T.pack <$> (space *> some (alphaNumChar <|> oneOf' "_№"))
