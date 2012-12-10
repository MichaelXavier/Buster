module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
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
main = do configFile <- listToMaybe <$> getArgs
          maybe noConfigError runWithPath configFile
          debugM "Sleeping main thread indefinitely"
          forever $ threadDelay maxBound
          --TODO: debug the blocked indefinitely on MVar error
  where noConfigError = failWith "Specify config file" >> error "nah dude"


runWithPath :: FilePath -> IO ()
runWithPath path = either badConfigError run =<< loadConfig path
  where badConfigError msg = failWith msg >> error "nah dude"

-- TODO: trap sigints and exit successfully
run :: Config -> IO ()
run cfg = do configureLogger $ configVerbose cfg
             infoM "Starting Buster"
             pool <- newPool cfg
             startPool pool
             infoM "Pool started"

failWith :: String -> IO ()
failWith msg = hPutStrLn stderr msg >> exitFailure
