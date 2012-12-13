module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
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

import Buster.Pool (newPool, startPool)
import Buster.Config (loadConfig)
import Buster.Types
import Buster.Logger

--TODO: use eitherT
main :: IO ()
main = runScript $ do
         args       <- scriptIO getArgs 
         configFile <- tryHead "Specify a config file" args
         scriptIO $ do runWithPath configFile
                       debugM "Sleeping main thread indefinitely"
                       waitForever

runWithPath :: FilePath -> IO ()
runWithPath path = runScript $ do config  <- scriptIO $ loadConfig path
                                  config' <- hoistEither config
                                  scriptIO $ run config'

-- TODO: trap sigints and exit successfully
run :: Config -> IO ()
run cfg = do configureLogger $ configVerbose cfg
             infoM "Starting Buster"
             pool <- newPool cfg
             startPool pool
             infoM "Pool started"

waitForever :: IO ()
waitForever = forever $ threadDelay maxBound
