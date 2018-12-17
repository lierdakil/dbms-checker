{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module API.TH where

import Language.Haskell.TH.Syntax
import GHC.Generics (Generic)
import Data.Swagger (ToSchema(..), ToParamSchema(..))
import API.JsonDeriv
import Data.Aeson.TH
import Servant (FromHttpApiData)
import Control.Monad
import Data.Proxy

deriveToSchema :: Name -> Q [Dec]
deriveToSchema t = let typ = return $ ConT t in
  [d|instance ToSchema $typ|]

deriveGeneric :: Name -> Q [Dec]
deriveGeneric t = let typ = return $ ConT t in
  [d|deriving instance Generic $typ|]

deriveToParamSchema :: Name -> Q [Dec]
deriveToParamSchema t = let typ = return $ ConT t in [d|instance ToParamSchema $typ|]

deriveFromHttpApiData :: Name -> Q [Dec]
deriveFromHttpApiData t = let typ = return $ ConT t
  in liftM2 (<>) [d|deriving instance FromHttpApiData $typ|] [d|deriving instance Read $typ|]

deriveJSONAndSchema :: Name -> Q [Dec]
deriveJSONAndSchema t = do
  d1 <- deriveJSONOnly t
  d2 <- deriveToSchema t
  return $ d1 <> d2

deriveJSONOnly :: Name -> Q [Dec]
deriveJSONOnly = deriveJSON jsonDerivationOptions

makeProxy :: Name -> Q Exp
makeProxy t = [|Proxy :: Proxy $typ|]
  where typ = return $ ConT t
