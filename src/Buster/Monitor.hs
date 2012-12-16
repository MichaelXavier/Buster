module Buster.Monitor (monitorForUpdates,
                       WatchDescriptor,
                       stopMonitoring) where

import Control.Monad (void)
import System.INotify

-- block until the file is updated
monitorForUpdates :: FilePath -> IO () -> IO WatchDescriptor
monitorForUpdates path callback = withINotify $ \inotify -> do
  addWatch inotify eventVarieties path callback'
  where eventVarieties = [Modify, CloseWrite]
        callback'      = const callback

stopMonitoring :: WatchDescriptor -> IO ()
stopMonitoring = removeWatch
