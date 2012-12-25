module Buster.Monitor (monitorForUpdates,
                       WatchDescriptor,
                       INotify,
                       withMonitoring,
                       installMonitor,
                       uninstallMonitor,
                       stopMonitoring) where

import System.INotify

import Buster.Config (reloadConfig)
import Buster.Logger
import Buster.Types

withMonitoring :: (INotify -> IO a) -> IO a
withMonitoring = withINotify

monitorForUpdates :: INotify -> FilePath -> IO () -> IO WatchDescriptor
monitorForUpdates inotify path callback = 
  addWatch inotify eventVarieties path callback'
  where eventVarieties = [Modify]
        callback' _     = callback

stopMonitoring :: WatchDescriptor -> IO ()
stopMonitoring = removeWatch

installMonitor :: INotify -> FilePath -> ConfigWatch -> IO WatchDescriptor
installMonitor inotify path configWatch = do
  debugM $ "Installing monitor for " ++ path
  desc <- monitorForUpdates inotify path callback
  debugM $ "Monitor installed for " ++ path
  return desc
  where callback = logUpdate >> reloadConfig path configWatch
        logUpdate = debugM $ "File " ++ path ++ " updated"

uninstallMonitor :: FilePath -> WatchDescriptor -> IO ()
uninstallMonitor path monitor = do debugM $ "Uninstalling monitor for " ++ path
                                   stopMonitoring monitor
