{-# LANGUAGE CPP #-}
module Buster.Util (decodeFileEither,
                    SomeException,
                    Exception,
                    pokemonException,
                    tryPokemonIO) where

import Control.Applicative
import Control.Exception (throwIO)
import Control.Error (fmapL)
import Data.Yaml (FromJSON, decodeHelper)
import qualified Text.Libyaml as Y (decodeFile)

-- | For when you gotta catch 'em all
#ifdef __GLASGOW_HASKELL__
import Control.Exception hiding (catch)
import System.Exit
import qualified Control.Exception (catch)
#else
-- Stub types and classes for Haskell98
type SomeException = IOError

class Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e

instance Exception IOError where
  toException = id
  fromException = Just
#endif

decodeFileEither :: FromJSON a => FilePath -> IO (Either String a)
decodeFileEither fp = decodeHelper (Y.decodeFile fp) >>= either throwIO return

-- | Describes the class of exceptions that should be caught when you
--   need to catch 'em all.
pokemonException :: SomeException -> SomeException
#ifdef __GLASGOW_HASKELL__
pokemonException e =
  let Just r = fmap rethrowArithException (fromException e) <|>
               fmap rethrowErrorCall (fromException e) <|>
               fmap rethrowExitCode (fromException e) <|>
               fmap rethrowArrayException (fromException e) <|>
               fmap rethrowAsyncException (fromException e) <|>
               fmap rethrowAssertionFailed (fromException e) <|>
               fmap rethrowDeadlock (fromException e) <|>
               fmap rethrowBlockedIndefinitelyOnSTM (fromException e) <|>
               fmap rethrowBlockedIndefinitelyOnMVar (fromException e) <|>
               fmap rethrowNestedAtomically (fromException e) <|>
               fmap rethrowNoMethodError (fromException e) <|>
               fmap rethrowRecUpdError (fromException e) <|>
               fmap rethrowRecConError (fromException e) <|>
               fmap rethrowRecSelError (fromException e) <|>
               fmap rethrowPatternMatchFail (fromException e) <|>
               Just e
  in
    r
  where
  rethrowArithException = throw :: ArithException -> a
  rethrowErrorCall = throw :: ErrorCall -> a
  rethrowExitCode = throw :: ExitCode -> a
  rethrowArrayException = throw :: ArrayException -> a
  rethrowAsyncException = throw :: AsyncException -> a
  rethrowAssertionFailed = throw :: AssertionFailed -> a
  rethrowDeadlock = throw :: Deadlock -> a
  rethrowBlockedIndefinitelyOnSTM = throw :: BlockedIndefinitelyOnSTM -> a
  rethrowBlockedIndefinitelyOnMVar = throw :: BlockedIndefinitelyOnMVar -> a
  rethrowNestedAtomically = throw :: NestedAtomically -> a
  rethrowNoMethodError = throw :: NoMethodError -> a
  rethrowRecUpdError = throw :: RecUpdError -> a
  rethrowRecConError = throw :: RecConError -> a
  rethrowRecSelError = throw :: RecSelError -> a
  rethrowPatternMatchFail = throw :: PatternMatchFail -> a
#else
-- Nothing to rethrow in Haskell98
pokemonException = id
#endif

-- | Catch 'em all and produce 'Either'
tryPokemonIO :: IO a -> IO (Either SomeException a)
tryPokemonIO io = fmap Right io
#ifdef __GLASGOW_HASKELL__
  `Control.Exception.catch`
#else
  `catch`
#endif
  (\e -> return $! Left $! pokemonException e)

-- If you catch one, do this instead
instance Alternative IO where
  empty = ioError (userError "IO Alternative empty")
  a <|> b = a
#ifdef __GLASGOW_HASKELL__
    `Control.Exception.catch`
#else
    `catch`
#endif
    (\e -> (return $! pokemonException e) >> b)
