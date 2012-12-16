module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
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
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (SignalSet, addSignal, emptySignalSet, awaitSignal, sigHUP, blockSignals, reservedSignals, installHandler, Handler(..))

import Buster.Pool (newPool, startPool, stopPool)
import Buster.Config (loadConfig)
import Buster.Types
import Buster.Logger

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

          run configMV

run :: MVar Config -> IO ()
run configMV = runScript $ scriptIO $ runWithConfig configMV =<< takeMVar configMV

runWithConfig :: MVar Config -> Config -> IO ()
runWithConfig configMV cfg = do stoppedPool  <- newPool cfg
                                pool         <- startPool stoppedPool
                                newCfg       <- takeMVar configMV
                                stopPool pool
                                runWithConfig configMV newCfg

reloadConfig :: FilePath -> MVar Config -> IO ()
reloadConfig configFile configMV = runScript $ do
  scriptIO $ debugM "Config file reloading"
  config' <- scriptIO $ loadConfig configFile
  config  <- hoistEither config'
  scriptIO $ do
    debugM "Config file successfully parsed"
    debugM "Reconfiguring Logger"
    configureLogger $ configVerbose config
    putMVar configMV config
