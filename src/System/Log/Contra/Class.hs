{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Log.Contra.Class
  ( HasLog,
    getLogger,
    scoped,
    LoggerT,
    runLoggerT,
    logMsg,
    trace,
    debug,
    info,
    warn,
    err,
    critical,
    M.Message,
    M.getValue,
    M.setValue,
    M.withValue,
    M.withValueM,
    M.update,
    M.getThreadId,
    M.withThreadId,
    M.Severity (..),
    M.getSeverity,
    M.minSeverity,
    M.withSeverity,
    M.getCallStack,
    M.withCallStack,
    module System.Log.Contra.Internal,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(ReaderT), ask, lift, local, runReaderT, MonadReader)
import Control.Monad.Trans (MonadTrans)
import Data.Functor.Contravariant (contramap)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import System.Log.Contra.Internal
import qualified System.Log.Contra.Message as M
import qualified System.Log.Contra.Severity as M

class HasLog m where
  getLogger :: m (Log m M.Message)
  scoped :: (M.Message -> M.Message) -> m a -> m a

newtype LoggerT m a = LoggerT {unLoggerT :: ReaderT (Log m M.Message) m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader r m => MonadReader r (LoggerT m) where
  ask = LoggerT $ ReaderT $ pure ask
  local f (LoggerT (ReaderT action)) = LoggerT $ ReaderT $ local f . action

runLoggerT :: Log m M.Message -> LoggerT m a -> m a
runLoggerT logger action = runReaderT (unLoggerT action) logger

instance MonadTrans LoggerT where
  lift m = LoggerT (lift m)

instance (Monad m) => HasLog (LoggerT m) where
  getLogger = LoggerT $ do
    (Log logger) <- ask
    pure $ Log (lift . logger)

  scoped f (LoggerT action) = LoggerT $ local (contramap f) action

logMsg :: (Monad m, HasLog m, Typeable a) => a -> m ()
logMsg value = getLogger >>= \logger -> M.logMsg logger value

trace :: (Monad m, HasLog m, Typeable a, HasCallStack) => a -> m ()
trace value = getLogger >>= \logger -> M.trace logger value

debug :: (Monad m, HasLog m, Typeable a, HasCallStack) => a -> m ()
debug value = getLogger >>= \logger -> M.debug logger value

info :: (Monad m, HasLog m, Typeable a, HasCallStack) => a -> m ()
info value = getLogger >>= \logger -> M.info logger value

warn :: (Monad m, HasLog m, Typeable a, HasCallStack) => a -> m ()
warn value = getLogger >>= \logger -> M.warn logger value

err :: (Monad m, HasLog m, Typeable a, HasCallStack) => a -> m ()
err value = getLogger >>= \logger -> M.err logger value

critical :: (Monad m, HasLog m, Typeable a, HasCallStack) => a -> m ()
critical value = getLogger >>= \logger -> M.critical logger value
