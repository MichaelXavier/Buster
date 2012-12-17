module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay,
                           killThread,
                           forkIO,
                           ThreadId)
import Control.Concurrent.MVar
import Control.Error (hoistEither)
import Control.Error.Script (Script, runScript, scriptIO)
import Control.Error.Safe (tryHead)
import Control.Monad (forever)
-- sigh
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Posix.Signals (SignalSet, addSignal, emptySignalSet, awaitSignal, sigHUP, blockSignals, reservedSignals, installHandler, Handler(..))

import Buster.Pool (newPool, startPool, stopPool)
import Buster.Config (loadConfig)
import Buster.Types
import Buster.Logger
import Buster.Monitor

main :: IO ()
main = runScript $ do
         args       <- scriptIO getArgs 
         configFile <- tryHead "Specify a config file" args

         scriptIO $ do
          configMV   <- newEmptyMVar

          debugM "Loading initial config"

          reloadConfig configFile configMV

          debugM "Installing Signal Handlers"
          blockSignals reservedSignals
          installHandler sigHUP (Catch $ reloadConfig configFile configMV) Nothing

          run configFile configMV

run :: FilePath -> MVar Config -> IO ()
run configFile configMV = runScript $ scriptIO $ runWithConfig configFile configMV =<< takeMVar configMV

runWithConfig :: FilePath -> MVar Config -> Config -> IO ()
runWithConfig configFile configMV cfg = do 
  stoppedPool   <- newPool cfg
  pool          <- startPool stoppedPool
  newCfg <- withMonitoring $ \inotify -> do
    mayWatchDesc  <- if configMonitor cfg
                      then 
                        Just <$> installMonitor inotify configFile configMV
                      else return Nothing
    newCfg        <- takeMVar configMV

    case mayWatchDesc of
      Just desc -> uninstallMonitor configFile desc
      _         -> return ()
    return newCfg

  stopPool pool
  runWithConfig configFile configMV newCfg


installMonitor :: INotify -> FilePath -> MVar Config -> IO WatchDescriptor
installMonitor inotify path configMV = do debugM $ "Installing monitor for " ++ path
                                          desc <- monitorForUpdates inotify path callback
                                          debugM $ "Monitor installed for " ++ path
                                          return desc

  where callback = logUpdate >> reloadConfig path configMV
        logUpdate = debugM $ "File " ++ path ++ " updated"

uninstallMonitor :: FilePath -> WatchDescriptor -> IO ()
uninstallMonitor path monitor = do debugM $ "Uninstalling monitor for " ++ path
                                   stopMonitoring monitor

reloadConfig :: FilePath -> MVar Config -> IO ()
reloadConfig configFile configMV = runScript $ do
  scriptIO $ debugM "Config file reloading"
  config' <- scriptIO $ loadConfig configFile
  config  <- hoistEither config'
  scriptIO $ do
    debugM "Config file successfully parsed"
    debugM "Reconfiguring Logger"
    configureLogger $ configVerbose config
    debugM "Reconfiguring Logger"
    putMVar configMV config
