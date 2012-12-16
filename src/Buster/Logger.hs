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
import System.IO (stdout)
import System.Log.Logger (updateGlobalLogger,
                          Priority(..),
                          rootLoggerName,
                          setLevel,
                          setHandlers)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (verboseStreamHandler)
import qualified System.Log.Logger as L

configureLogger :: Bool -> IO ()
configureLogger verbose = do handler <- setFormatter <$> defaultHandler <*> pure defaultFormatter
                             setLogHandler handler
                             setLogLevel

                             
  where defaultHandler = verboseStreamHandler stdout logLevel 
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
