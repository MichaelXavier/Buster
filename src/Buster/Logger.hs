module Buster.Logger (configureLogger,
                      defaultLog,
                      debugM,
                      infoM,
                      noticeM,
                      warningM,
                      errorM,
                      criticalM,
                      alertM,
                      emergencyM) where

import Control.Applicative ((<$>), (<*>), pure)
import System.IO (stdout, openFile, IOMode(..))
import System.Log.Logger (updateGlobalLogger,
                          Priority(..),
                          rootLoggerName,
                          setLevel,
                          setHandlers)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (verboseStreamHandler)
import qualified System.Log.Logger as L

configureLogger :: Maybe FilePath -> Bool -> IO ()
configureLogger logFile verbose = do handle <- getHandle 
                                     handler <- setFormatter <$> defaultHandler handle <*> pure defaultFormatter
                                     setLogHandler handler
                                     setLogLevel

                             
  where defaultHandler handle = verboseStreamHandler handle logLevel 
        getHandle             = case logFile of
                                  Just path -> openFile path AppendMode
                                  _         -> return stdout
        setLogHandler handler = updateGlobalLogger defaultLog $ setHandlers [handler]
        setLogLevel           = updateGlobalLogger defaultLog $ setLevel logLevel
        defaultFormatter      = simpleLogFormatter defaultFormat
        defaultFormat         = "$time [$prio] $msg"
        logLevel              = if verbose
                                  then DEBUG
                                  else WARNING

defaultLog :: String
defaultLog = rootLoggerName

debugM :: String -> IO ()
debugM = L.debugM defaultLog

infoM :: String -> IO ()
infoM = L.infoM defaultLog

noticeM :: String -> IO ()
noticeM = L.noticeM defaultLog

warningM :: String -> IO ()
warningM = L.warningM defaultLog

errorM :: String -> IO ()
errorM = L.errorM defaultLog

criticalM :: String -> IO ()
criticalM = L.criticalM defaultLog

alertM :: String -> IO ()
alertM = L.alertM defaultLog

emergencyM :: String -> IO ()
emergencyM = L.emergencyM defaultLog
