{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Algo.RSTools.Pretty where

import Algo.RSTools.Types
import Algo.FDTools.Pretty
import qualified Data.HashSet as S
import qualified Data.Text.Lazy as T

relToString :: Relation -> T.Text
relToString (Relation attrs)
  = "(" <> T.intercalate ", " (map attrToString (S.toList attrs)) <> ")"

attrToString :: Attribute -> T.Text
attrToString Attribute{..} =
    (if attributeIsKey then "*" else "") <>
    vertexToString attributeName <> " : " <>
    domainToString attributeDomain

domainToString :: Domain -> T.Text
domainToString DomainString = "строка"
domainToString DomainText = "текст"
domainToString DomainDate = "дата"
domainToString DomainTime = "время"
domainToString DomainDateTime = "дата/время"
domainToString (DomainNumber Whole) = "целое"
domainToString (DomainNumber Natural) = "натуральное"
domainToString (DomainNumber Rational) = "дробное"
domainToString (DomainEnum elems) = "перечисление (" <> T.intercalate ", " (S.toList elems) <> ")"
domainToString (DomainOther name) = name
