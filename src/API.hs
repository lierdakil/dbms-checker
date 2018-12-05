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
import API.Instances ()
import GHC.Types (Symbol)
import Servant.Swagger.UI

type BasicAPI =
       "auth" :> ReqBody '[JSON] AuthData :> Post '[JSON] UserSessionData
  :<|> "predefined-topics" :> Get '[JSON] [PredefinedTopic]
  :<|> "custom-topics" :> (
             Post '[JSON] Text
        :<|> Capture "topicId" CustomTopicIdentifier :> (
                  ReqBody '[JSON] Text :> Put '[JSON] ()
             :<|> ReqBody '[JSON] AcceptanceState :> Patch '[JSON] ()
             )
        )
  :<|> "users" :> (
             Capture "userId" UserIdentifier :> (
                  "topic" :> (
                         Get '[JSON] AssignedTopicInfo
                    :<|> ReqBody '[JSON] AssignedTopic :> Put '[JSON] AssignedTopicInfo
                    )
             :<|> "erd" :> BasicGet ERDIdentifier
             :<|> "fundep" :> BasicGet FunDepIdentifier
             :<|> "relschema" :> BasicGet RelSchemaIdentifier
             :<|> "sqlschema" :> BasicGet PhysSchemaIdentifier
             )
        )
  :<|> "erd" :> BasicCrud "erdId" ERDIdentifier
  :<|> "fundeps" :> BasicCrud "fundepId" FunDepIdentifier
  :<|> "relschemas" :> BasicCrud "relschemaId" RelSchemaIdentifier
  :<|> "sqlschemas" :> BasicCrud "sqlschemaId" PhysSchemaIdentifier
  :<|> "comments" :> (
            ReqBody '[JSON] CommentBodyInfo :> Post '[JSON] CommentInfo
       :<|> QueryParam "parentItem" ParentItemIdentifier :> Get '[JSON] [CommentInfo]
       :<|> Capture "commentId" CommentIdentifier :> (
                 ReqBody '[JSON] CommentBodyInfo :> Put '[JSON] CommentInfo
            :<|> ReqBody '[JSON] CommentStatusInfo :> Patch '[JSON] ()
            )
       )

type BasicGet (idType :: *) =
       Get '[JSON] (BasicCrudResponseBody idType)

type BasicCrud (idName :: Symbol) (idType :: *) =
       Get '[JSON] (BasicCrudResponseBody idType)
  :<|> ReqBody '[JSON] Text :> Post '[JSON] (BasicCrudResponseBody idType)
  :<|> Capture idName idType :> BasicManip idType

type family CanRender  (idType :: *) where
  CanRender ERDIdentifier = 'True
  CanRender FunDepIdentifier = 'True
  CanRender idType = 'False

type BasicManip (idType :: *) =
    ExtendWithRender idType (
      ExtendWithAccept idType (
        ReqBody '[JSON] Text :> Put '[JSON] (BasicCrudResponseBody idType)
      )
    )

type ExtendWithRender (idType :: *) (a :: *) = ExtendWithRenderInternal (CanRender idType) a

type family ExtendWithRenderInternal (canRender :: Bool) (a :: *) where
  ExtendWithRenderInternal 'True a = a :<|> "render" :> Get '[OctetStream] FileData
  ExtendWithRenderInternal 'False a = a

type ExtendWithAccept (idType :: *) (a :: *) = ExtendWithAcceptInternal (CanAccept idType) a

type family ExtendWithAcceptInternal (canAccept :: Bool) (a :: *) where
  ExtendWithAcceptInternal 'True a = a :<|> ReqBody '[JSON] AcceptanceState :> Patch '[JSON] ()
  ExtendWithAcceptInternal 'False a = a

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
