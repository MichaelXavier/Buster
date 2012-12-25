module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (takeMVar)
import Control.Error.Script (runScript,
                             scriptIO)
import Control.Error.Safe (tryHead)
import System.Environment (getArgs)

import Buster.Pool (newPool,
                    startPool,
                    stopPool)
import Buster.Config (installReloadHandler,
                      reloadConfig,
                      setupConfigWatch)
import Buster.Types
import Buster.Logger
import Buster.Monitor

main :: IO ()
main = runScript $ do
         args       <- scriptIO getArgs 
         configFile <- tryHead "Specify a config file" args

         scriptIO $ do
          configWatch <- setupConfigWatch

          debugM "Loading initial config"

          reloadConfig configFile configWatch

          debugM "Installing Signal Handlers"

          installReloadHandler configFile configWatch

          run configFile configWatch

--TODO: takMVar is a leaky abstraction here
run :: FilePath -> ConfigWatch -> IO ()
run configFile configWatch = runScript $ scriptIO $ runWithConfig configFile configWatch =<< takeMVar configWatch

runWithConfig :: FilePath -> ConfigWatch -> Config -> IO ()
runWithConfig configFile configWatch cfg = do 
  stoppedPool   <- newPool cfg
  pool          <- startPool stoppedPool
  newCfg <- withMonitoring $ \inotify -> do
    mayWatchDesc  <- if configMonitor cfg
                      then 
                        Just <$> installMonitor inotify configFile configWatch
                      else return Nothing
    newCfg        <- takeMVar configWatch

    case mayWatchDesc of
      Just desc -> uninstallMonitor configFile desc
      _         -> return ()
    return newCfg

  stopPool pool
  runWithConfig configFile configWatch newCfg
