{-# LANGUAGE OverloadedStrings #-}
module Buster.Types (Config(..),
                     UrlConfig(..),
                     Worker,
                     BusterPool(..)) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (ThreadId)
--import Data.IORef
import Data.List (intercalate)
import Data.Yaml (FromJSON(..), (.:?), (.:), (.!=), Value(..))
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Types (Method)

data Config = Config {
  configVerbose :: Bool,
  urlConfigs :: [UrlConfig]
} deriving (Show, Eq)

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .:? "verbose" .!= False
                                <*> v .: "urls"
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
                    workers = ws} = intercalate " " ["BusterPool",
                                                     show cfg,
                                                     show ws]

type Worker = ThreadId
