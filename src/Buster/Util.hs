module Buster.Util (decodeFileEither) where

import Control.Exception (throwIO)
import Data.Yaml (FromJSON, decodeHelper)
import qualified Text.Libyaml as Y (decodeFile)

decodeFileEither :: FromJSON a => FilePath -> IO (Either String a)
decodeFileEither fp = decodeHelper (Y.decodeFile fp) >>= either throwIO return
