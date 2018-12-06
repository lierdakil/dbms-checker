{-# LANGUAGE DuplicateRecordFields, TypeFamilies, DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module API.Instances () where

import API.TH

import GHC.Generics (Generic)
import Servant.Auth.Server
import DB.Types
import API.Types
import Control.Lens
import Data.Text (pack, unpack)
import Text.Read
import Control.Arrow
import Servant (MimeRender, OctetStream, FromHttpApiData(..))
import Data.Swagger (ToSchema(..))
import qualified Data.Swagger as S
import Data.Swagger.Schema (genericDeclareNamedSchemaUnrestricted, defaultSchemaOptions)

import API.TypeLists

deriving instance MimeRender OctetStream FileData
instance ToSchema FileData where
  declareNamedSchema _ = return $ S.NamedSchema Nothing S.binarySchema

instance FromJWT UserSessionData
instance ToJWT UserSessionData

$(mconcat <$> traverse deriveJSONOnly jsonOnlyTypes)

deriving instance Generic ParentComment
instance ToSchema ParentComment where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

deriving instance Generic (BasicCrudResponseBodyWithoutAnything a)
deriving instance Generic (BasicCrudResponseBodyWithValidation a)
deriving instance Generic (BasicCrudResponseBodyWithAcceptance a)
deriving instance Generic (BasicCrudResponseBodyWithAcceptanceAndValidation a)
instance (ToSchema a) => ToSchema (BasicCrudResponseBodyWithoutAnything a)
instance (ToSchema a) => ToSchema (BasicCrudResponseBodyWithValidation a)
instance (ToSchema a) => ToSchema (BasicCrudResponseBodyWithAcceptance a)
instance (ToSchema a) => ToSchema (BasicCrudResponseBodyWithAcceptanceAndValidation a)


$(mconcat <$> traverse deriveJSONAndSchema jsonAndSchemaTypes)

$(mconcat <$> traverse deriveToParamSchema [
    ''UserIdentifier
  , ''PredefinedTopicIdentifier
  , ''CustomTopicIdentifier
  , ''ERDIdentifier
  , ''CommentIdentifier
  , ''FunDepIdentifier
  , ''RelSchemaIdentifier
  , ''PhysSchemaIdentifier
  -- , ''ParentItemIdentifier
  ])

instance S.ToParamSchema ParentItemIdentifier where
  toParamSchema _ = mempty
     & S.type_ .~ S.SwaggerString

$(mconcat <$> traverse deriveFromHttpApiData [
    ''UserIdentifier
  , ''PredefinedTopicIdentifier
  , ''CustomTopicIdentifier
  , ''ERDIdentifier
  , ''CommentIdentifier
  , ''FunDepIdentifier
  , ''RelSchemaIdentifier
  , ''PhysSchemaIdentifier
  ])

deriving instance Read ParentItemIdentifier
instance FromHttpApiData ParentItemIdentifier where
  parseQueryParam = left pack . readEither . unpack
