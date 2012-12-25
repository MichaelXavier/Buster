module Buster.Monitor (monitorForUpdates,
                       WatchDescriptor,
                       INotify,
                       withMonitoring,
                       stopMonitoring) where

import Control.Monad (void)
import System.INotify

withMonitoring :: (INotify -> IO a) -> IO a
withMonitoring = withINotify

-- block until the file is updated
monitorForUpdates :: INotify -> FilePath -> IO () -> IO WatchDescriptor
monitorForUpdates inotify path callback = 
  addWatch inotify eventVarieties path callback'
  where eventVarieties = [Modify]
        callback' _     = callback

stopMonitoring :: WatchDescriptor -> IO ()
stopMonitoring = removeWatch
