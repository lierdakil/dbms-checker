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
import Servant.Auth.Server
import DB.Types
import API.Types
import API.Instances ()
import GHC.Types (Symbol)
import Servant.Swagger.UI
import qualified Data.ByteString.Lazy as BL

type CustomTopicsAPI =
       ReqBody '[JSON] Text :> Post '[JSON] AssignedTopicInfo
  :<|> Capture "topicId" CustomTopicIdentifier :> (
            ReqBody '[JSON] Text :> Put '[JSON] (Maybe AssignedTopicInfo)
       :<|> ReqBody '[JSON] AcceptanceState :> Patch '[JSON] ()
       )

type UsersAPI =
  Capture "userId" UserIdentifier :> (
       "topic" :> (
              Get '[JSON] (Maybe AssignedTopicInfo)
         :<|> ReqBody '[JSON] AssignedTopic :> Put '[JSON] AssignedTopicInfo
         )
  :<|> "erd" :> BasicGet ERDIdentifier
  :<|> "fundep" :> BasicGet FunDepIdentifier
  :<|> "relschema" :> BasicGet RelSchemaIdentifier
  :<|> "sqlschema" :> BasicGet PhysSchemaIdentifier
  )

type CommentsAPI =
       ReqBody '[JSON] CommentBodyInfo :> Post '[JSON] CommentInfo
  :<|> QueryParam "parentItem" ParentItemIdentifier :> Get '[JSON] [CommentInfo]
  :<|> Capture "commentId" CommentIdentifier :> (
            ReqBody '[JSON] CommentBodyInfo :> Put '[JSON] CommentInfo
       :<|> ReqBody '[JSON] CommentStatus :> Patch '[JSON] ()
       )

type BasicAPI =
       "predefined-topics" :> Get '[JSON] [PredefinedTopic]
  :<|> "custom-topics" :> CustomTopicsAPI
  :<|> "users" :> UsersAPI
  :<|> "erd" :> BasicCrud "erdId" ERDIdentifier
  :<|> "fundeps" :> BasicCrud "fundepId" FunDepIdentifier
  :<|> "relschemas" :> BasicCrud "relschemaId" RelSchemaIdentifier
  :<|> "sqlschemas" :> BasicCrud "sqlschemaId" PhysSchemaIdentifier
  :<|> "comments" :> CommentsAPI
  :<|> "render" :> Capture "type" Text
                :> ReqBody '[JSON] Text :> Post '[OctetStream] FileData

type BasicGet (idType :: *) =
       Get '[JSON] (Maybe (BasicCrudResponseBody idType))

type BasicCrud (idName :: Symbol) (idType :: *) =
       ReqBody '[JSON] Text :> Post '[JSON] (BasicCrudResponseBody idType)
  :<|> Capture idName idType :> BasicManip idType

type BasicManip (idType :: *) =
    ExtendWithAccept idType (
      Get '[JSON] (BasicCrudResponseBody idType)
 :<|> ReqBody '[JSON] Text :> Put '[JSON] (BasicCrudResponseBody idType)
    )

type ExtendWithAccept (idType :: *) (a :: *) = ExtendWithAcceptInternal (CanAccept idType) a

type family ExtendWithAcceptInternal (canAccept :: Bool) (a :: *) where
  ExtendWithAcceptInternal 'True a = ReqBody '[JSON] AcceptanceState :> Patch '[JSON] () :<|> a
  ExtendWithAcceptInternal 'False a = a

type MainAPI = Auth '[JWT] UserSessionData :> BasicAPI

type LoginAPI = "auth" :> ReqBody '[JSON] AuthData :> Post '[JSON] UserSessionData

type API = MainAPI
      :<|> LoginAPI
      :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"
      :<|> Raw

basicApi :: Proxy BasicAPI
basicApi = Proxy

loginApi :: Proxy LoginAPI
loginApi = Proxy

mainApi :: Proxy MainAPI
mainApi = Proxy

api :: Proxy API
api = Proxy
