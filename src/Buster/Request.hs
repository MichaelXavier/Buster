module Buster.Request (makeRequest) where

import Control.Applicative ((<$>))
import Control.Error
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Exception as E
import Data.Char (toUpper)
import Data.Conduit (runResourceT)
import Data.Ix (inRange)
import Data.Monoid (mconcat)
import qualified Data.ByteString.Char8 as BS8 (unpack)
import Network.HTTP.Conduit (Manager,
                             Request(..),
                             parseUrl,
                             httpLbs,
                             Response(..),
                             HttpException)
import Network.HTTP.Types (Status(..))

import Buster.Types
import Buster.Logger

makeRequest :: Manager -> UrlConfig -> IO ()
makeRequest mgr urlConfig = do
  resp <- tryIOMsg $ do
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

tryIOMsg :: IO a -> IO (Either String b)
tryIOMsg action = tryIO' action

tryIO' :: IO a -> IO (Either String b)
tryIO' action = action' `E.catches` handlers
  where action' :: IO (Either String b)
        action' = do x <- action
                     return (Right x)

handlerIO :: (E.IOException -> IO (Either String a)) -> E.Handler (Either String a)
handlerIO   = E.Handler

handlerHttp :: (HttpException -> IO (Either String a)) -> E.Handler (Either String a)
handlerHttp = E.Handler

handler :: (Show e, E.Exception e) => e -> IO (Either String a)
handler e   = return . Left . show $ e

handlers :: [E.Handler (Either String a)]
handlers    = [handlerIO handler, handlerHttp handler]
