{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module API where

import Servant
import Data.Text (Text)
import qualified Data.Text as T
import Servant.Auth.Server
import DBTypes
import API.Types
import GHC.Types (Symbol)
import Servant.Swagger.UI

type BasicAPI =
       "auth" :> ReqBody '[JSON] AuthData :> Post '[JSON] UserSessionData
  :<|> "topics" :> Get '[JSON] [PredefinedTopic]
  :<|> "topic" :> (
             Capture "userId" UserIdentifier :> (
                    Get '[JSON] AssignedTopicInfo
               :<|> ReqBody '[JSON] AssignedTopic :> Post '[JSON] ()
               )
        :<|> Capture "topicId" CustomTopicIdentifier
             :> ReqBody '[JSON] AcceptanceState :> Patch '[JSON] ()
        )
  :<|> "erd" :> (
          BasicCrud "erdId" ERDIdentifier 'True
     :<|> "render" :> Capture "erdId" ERDIdentifier :> Get '[OctetStream] FileData
     )
  :<|> "fundep" :> (
          BasicCrud "fundepId" FunDepIdentifier 'False
     :<|> "render" :> Capture "fundepId" FunDepIdentifier :> Get '[OctetStream] FileData
     )
  :<|> "relschema" :> BasicCrud "relschemaId" RelSchemaIdentifier 'False
  :<|> "sqlschema" :> BasicCrud "sqlschemaId" PhysSchemaIdentifier 'True

type Comments = "comments" :> Get '[JSON] [CommentInfo]
  :<|> "comment" :> (
          ReqBody '[JSON] CommentBodyInfo :> Post '[JSON] CommentInfo
     :<|> ReqBody '[JSON] CommentStatusInfo :> Patch '[JSON] ()
     )

type BasicCrud (idName :: Symbol) (idType :: *) (canAccept :: Bool) =
       Capture "userId" UserIdentifier :> Get '[JSON] (BasicCrudResponseBody idType canAccept)
  :<|> ReqBody '[JSON] Text :> Post '[JSON] (BasicCrudResponseBody idType canAccept)
  :<|> Capture idName idType :> BasicManip idName idType canAccept

type family BasicManip (idName :: Symbol) (idType :: *) (canAccept :: Bool) where
  BasicManip idName idType 'False = ReqBody '[JSON] Text :> Put '[JSON] ()
  BasicManip idName idType 'True =
         ReqBody '[JSON] Text :> Put '[JSON] ()
    :<|> ReqBody '[JSON] AcceptanceState :> Patch '[JSON] ()
    :<|> Comments

type MainAPI = Auth '[JWT] UserSessionData :> BasicAPI

type LoginAPI = "login" :> ReqBody '[JSON] AuthData :> Post '[JSON] T.Text

type API = (MainAPI :<|> LoginAPI)
      :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

basicApi :: Proxy BasicAPI
basicApi = Proxy

mainApi :: Proxy MainAPI
mainApi = Proxy

api :: Proxy API
api = Proxy
