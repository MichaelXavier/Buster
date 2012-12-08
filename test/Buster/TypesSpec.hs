{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Buster.TypesSpec (spec) where

import Test.Hspec
import Data.String.QQ (s)
import Data.Yaml (decodeEither)

import Buster.Types

spec :: Spec
spec = do
  describe "parsing Config" $ do
    it "parses a minimal Config" $ 
      parseStr baseConfigStr `shouldBe` Right baseConfig
    it "parses a full Config" $ 
      parseStr fullConfigStr `shouldBe` Right fullConfig
    it "defaults method" $
      parseStr fullConfigNoMethodStr `shouldBe` Right fullConfigGet
    it "parses multiple urls" $
      parseStr fullConfigMultipleStr `shouldBe` Right fullConfigMultiple
  where baseConfigStr = [s|
---
urls: []
        |]
        fullConfigStr = [s|
verbose: true
urls:
- url: http://www.example.com
  interval: 1000
  method: POST
        |]
        fullConfigNoMethodStr = [s|
verbose: true
urls:
- url: http://www.example.com
  interval: 1000
        |]
        fullConfigMultipleStr = [s|
verbose: true
urls:
- url: http://www.example.com
  interval: 1000
  method: POST
- url: http://www.example.com
  interval: 1000
  method: GET
        |]
        baseConfig = Config { configVerbose = False, urlConfigs = []}
        fullConfigGet = Config { configVerbose = True,
                                 urlConfigs = [fullUrlConfig { requestMethod = "GET"}]}
        fullConfig = Config { configVerbose = True,
                              urlConfigs = [fullUrlConfig]}
        fullConfigMultiple = Config { configVerbose = True,
                                      urlConfigs = [fullUrlConfig,
                                                    fullUrlConfig { requestMethod = "GET"}]}
        fullUrlConfig = UrlConfig { url             = "http://www.example.com",
                                    requestInterval = 1000,
                                    requestMethod   = "POST" }
        parseStr = decodeEither
