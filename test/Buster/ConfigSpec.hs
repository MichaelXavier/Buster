{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Buster.ConfigSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (hPut, writeFile)
import Data.String.QQ (s)
import System.IO (hFlush)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Test.Hspec.Expectations (shouldReturn)

import Buster.Types
import Buster.Config

spec :: Spec
spec = do
  describe "parsing from file" $ do
    it "parses a full config successfully" $ do
      withPreloadedFile fullConfigStr $ \path -> do
        loadConfig path `shouldReturn` Right fullConfig
  where fullConfigStr = [s|
verbose: true
monitor: true
log_file: /path/to/output.log
urls:
- url: http://www.example.com
  interval: 1000
  method: POST
        |]
        fullConfig = Config { configVerbose = True,
                              configMonitor = True,
                              configLogFile = Just "/path/to/output.log",
                              urlConfigs = [fullUrlConfig]}
        fullUrlConfig = UrlConfig { url             = "http://www.example.com",
                                    requestInterval = 1000,
                                    requestMethod   = "POST" }



withPreloadedFile :: ByteString -> (FilePath -> IO a) -> IO a
withPreloadedFile content action = withSystemTempFile filenameTemplate callback
  where filenameTemplate     = "buster_fixture.yml"
        callback path handle = BS.hPut handle content >> hFlush handle >> action path
