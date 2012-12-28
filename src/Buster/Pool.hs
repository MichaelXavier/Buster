module Buster.Pool (stopPool,
                    newPool,
                    startPool) where

import Control.Applicative ((<$>),
                            (<*>),
                            pure)
import Control.Concurrent (killThread,
                           forkIO)
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (forever)
import Data.Default (def)
import Network.HTTP.Conduit (Manager,
                             newManager,
                             closeManager)

import Buster.Types
import Buster.Request (makeRequest)

stopPool :: BusterPool -> IO BusterPool
stopPool bp@BusterPool { workers = ws,
                         connectionManager = mgr } = do
  mapM_ killThread ws
  closeManager mgr
  return bp { workers = []}

newPool :: Config -> IO BusterPool
newPool cfg = BusterPool <$> newManager def
                         <*> pure cfg
                         <*> pure []

startPool :: BusterPool -> IO BusterPool
startPool bp@BusterPool { connectionManager = mgr,
                          config = Config { urlConfigs = cfgs}} = do startedWorkers <- startWorkers
                                                                     return bp { workers = startedWorkers }
  where startWorkers = mapM startWorker cfgs
        startWorker = buildWorker mgr


buildWorker :: Manager -> UrlConfig -> IO Worker
buildWorker mgr urlConfig = forkIO $ forever $ do
                              delay microseconds
                              makeRequest mgr urlConfig
  where microseconds = 1000000 * requestInterval urlConfig
