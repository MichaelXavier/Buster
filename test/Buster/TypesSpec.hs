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
        baseConfig = Config { configVerbose = False, urlConfigs = []}
        fullConfig = Config { configVerbose = True,
                              urlConfigs = [fullUrlConfig]}
        fullUrlConfig = UrlConfig { url             = "http://www.example.com",
                                    requestInterval = 1000,
                                    requestMethod   = "POST" }
        parseStr = decodeEither
