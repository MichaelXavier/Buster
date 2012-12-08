{-# LANGUAGE OverloadedStrings #-}
module Buster.Types (Config(..),
                     UrlConfig(..)) where

import Control.Applicative ((<$>), (<*>))
import Data.Yaml (FromJSON(..), (.:?), (.:), (.!=), Value(..))
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
