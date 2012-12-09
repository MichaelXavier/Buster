module Buster.Request (makeRequest) where

import Buster.Types
import Buster.Logger
import Network.HTTP.Conduit (Manager)

-- todo: logger, request manager
makeRequest :: Manager -> UrlConfig -> IO ()
makeRequest mgr urlConfig = debugM $ "Would request " ++ url urlConfig
