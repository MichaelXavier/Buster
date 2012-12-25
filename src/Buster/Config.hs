module Buster.Config (loadConfig,
                      setupConfigWatch,
                      reloadConfig,
                      summonConfig,
                      installReloadHandler) where

import Control.Concurrent.MVar (newEmptyMVar,
                                takeMVar,
                                putMVar)
import Control.Monad (void)
import Control.Error (runScript,
                      hoistEither,
                      scriptIO)
import System.Posix.Signals (sigHUP,
                             Handler(..),
                             blockSignals,
                             reservedSignals,
                             installHandler)

import Buster.Logger
import Buster.Types (Config(..),
                     ConfigWatch)
import Buster.Util (decodeFileEither)

loadConfig :: FilePath -> IO (Either String Config)
loadConfig = decodeFileEither

setupConfigWatch :: IO (ConfigWatch)
setupConfigWatch = newEmptyMVar

reloadConfig :: FilePath -> ConfigWatch -> IO ()
reloadConfig configFile configWatch = runScript $ do
  scriptIO $ debugM "Config file reloading"
  config' <- scriptIO $ loadConfig configFile
  config@Config { configVerbose = verbose,
                  configLogFile = logFile}  <- hoistEither config'
  scriptIO $ do
    debugM "Config file successfully parsed"
    debugM "Reconfiguring Logger"
    configureLogger logFile verbose
    debugM "Reconfiguring Logger"
    putMVar configWatch config

installReloadHandler :: FilePath -> ConfigWatch -> IO ()
installReloadHandler configFile configWatch = void $ do
  blockSignals reservedSignals
  installHandler sigHUP (Catch $ reloadConfig configFile configWatch) Nothing

summonConfig :: ConfigWatch -> IO (Config)
summonConfig = takeMVar
