module Buster.Pool (stopPool,
                    newPool,
                    restartPool,
                    startPool) where

import Control.Applicative ((<$>),(<*>), pure)
import Control.Concurrent (killThread, forkIO)
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (forever)
import Data.Default (def)
import Network.HTTP.Conduit (Manager, newManager, closeManager)

import Buster.Types
import Buster.Request (makeRequest)

stopPool :: BusterPool -> IO BusterPool
stopPool bp@BusterPool { workers = ws,
                         connectionManager = mgr } = do
  mapM killThread ws
  closeManager mgr
  return bp { workers = []}

newPool :: Config -> IO BusterPool
newPool cfg = BusterPool <$> newManager def
                         <*> pure cfg
                         <*> pure []

restartPool :: BusterPool -> IO BusterPool
restartPool bp = do stopPool bp
                    bp' <- newPool $ config bp
                    startPool bp'

startPool :: BusterPool -> IO BusterPool
startPool bp@BusterPool { connectionManager = mgr,
                          config = Config { urlConfigs = cfgs}} = do startedWorkers <- startWorkers
                                                                     return bp { workers = startedWorkers }
  where startWorkers = mapM startWorker cfgs
        startWorker = buildWorker mgr


--TODO: logger
buildWorker :: Manager -> UrlConfig -> IO Worker
buildWorker mgr urlConfig = forkIO $ forever $ do makeRequest mgr urlConfig
                                                  delay microseconds
  where microseconds = 1000000 * (requestInterval urlConfig)
