module Buster.Request (makeRequest) where

import Control.Error
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Exception.Base as E
import Data.Char (toUpper)
import Data.Conduit (runResourceT)
import Data.Ix (inRange)
import Data.Monoid (mconcat)
import qualified Data.ByteString.Char8 as BS8 (unpack)
import Network.HTTP.Conduit (Manager, Request(..), parseUrl, httpLbs, http, Response(..), withManager)
import Network.HTTP.Types (Status(..))

import Buster.Types
import Buster.Logger

makeRequest :: Manager -> UrlConfig -> IO ()
makeRequest mgr urlConfig = do
  resp <- runEitherT $ do
    tryIOMsg $ do
      debugM $ "Parsing " ++ url urlConfig
      req  <- generateRequest urlConfig
      debugM $ formatRequest urlConfig
      runResourceT $ httpLbs req mgr
  either logErrorMessage logSuccess resp
  where logSuccess      = logResponse urlConfig
        logErrorMessage = errorM

generateRequest :: UrlConfig -> IO (Request m')
generateRequest UrlConfig { url = u,
                            requestMethod = meth } = do req <- parseUrl u
                                                        return req { method = meth,
                                                                     checkStatus = \_ _ -> Nothing }

formatRequest :: UrlConfig -> String
formatRequest UrlConfig { url = u,
                          requestMethod = meth } = mconcat [meth', " ", u]
  where meth' = map toUpper $ BS8.unpack meth

logResponse :: UrlConfig -> Response a -> IO ()
logResponse urlConfig Response { responseStatus = Status { statusCode = code},
                                 responseBody = _ } = if success
                                                           then logSuccess
                                                           else logFailure
  where success = inRange (200, 399) code
        logSuccess = debugM formatMessage
        logFailure = errorM formatMessage
        formatMessage = mconcat [show code, " (", formatRequest urlConfig, ")"]

tryIOMsg :: (MonadIO m) => IO a -> EitherT String m a
tryIOMsg action = fmapLT show $ tryIO' action

-- Need to be able to catch everything since parsing URLs can throw
-- ParseExceptions. Why does Haskell even have exceptions :(
tryIO' :: (MonadIO m) => IO a -> EitherT E.SomeException m a
tryIO' = EitherT . liftIO . E.try
