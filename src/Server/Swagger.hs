module Server.Swagger where

import API
import Servant.Swagger
import Data.Swagger

swaggerDoc :: Swagger
swaggerDoc = toSwagger basicApi
