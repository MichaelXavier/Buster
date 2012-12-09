module Buster.Config (loadConfig) where

import System.IO (FilePath)

import Buster.Types (Config(..))
import Buster.Util (decodeFileEither)

loadConfig :: FilePath -> IO (Either String Config)
loadConfig = decodeFileEither
