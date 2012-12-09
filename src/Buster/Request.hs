module Buster.Request (makeRequest) where

import Buster.Types
import Network.HTTP.Conduit (Manager)

-- todo: logger, request manager
makeRequest :: Manager -> UrlConfig -> IO ()
makeRequest mgr urlConfig = putStrLn $ "WOULD REQUEST " ++ url urlConfig
