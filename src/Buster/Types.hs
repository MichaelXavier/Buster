{-# LANGUAGE OverloadedStrings #-}
module Buster.Types (Config(..),
                     UrlConfig(..),
                     Worker,
                     BusterPool(..)) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (ThreadId)
import Data.Yaml (FromJSON(..), (.:?), (.:), (.!=), Value(..))
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Types (Method)

data Config = Config {
  configVerbose :: Bool,
  configMonitor :: Bool,
  urlConfigs :: [UrlConfig],
  configLogFile :: Maybe FilePath
} deriving (Show, Eq)

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .:? "verbose" .!= False
                                <*> v .:? "monitor" .!= False
                                <*> v .:  "urls"
                                <*> v .:? "log_file"
  parseJSON _          = fail "Expecting Object"

data UrlConfig = UrlConfig {
  url :: String,
  requestInterval :: Integer,
  requestMethod :: Method 
} deriving (Show, Eq)

instance FromJSON UrlConfig where
  parseJSON (Object v) = UrlConfig <$> v .: "url"
                                   <*> v .: "interval"
                                   <*> v .:? "method" .!= "GET"
  parseJSON _          = fail "Expecting Object"

data BusterPool = BusterPool {
  connectionManager :: Manager,
  config            :: Config,
  workers           :: [Worker]
}

instance Show BusterPool where
  show BusterPool { config = cfg,
                    workers = ws} = unwords ["BusterPool",
                                             show cfg,
                                             show ws]

type Worker = ThreadId
