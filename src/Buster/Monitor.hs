module Buster.Monitor (monitorForUpdates) where

import Control.Monad (void)
import System.INotify

-- block until the file is updated
monitorForUpdates :: FilePath -> IO () -> IO ()
monitorForUpdates path callback = void $ withINotify $ \inotify -> do
  addWatch inotify eventVarieties path callback'
  where eventVarieties = [Modify]
        callback'      = const callback
