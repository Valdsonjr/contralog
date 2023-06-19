module System.Log.Contra.Severity where

import System.Log.Contra.Internal (Log, logTo, logWhen)

data Severity
  = Trace
  | Debug
  | Info
  | Warning
  | Error
  | Critical
  deriving (Read, Eq, Ord, Show)

withMinSeverity ::
  (Applicative m) =>
  Severity ->
  (a -> Severity) ->
  Log m a ->
  Log m a
withMinSeverity severity getter = logWhen ((>= severity) . getter)

logSeverity :: Severity -> (Severity -> a -> b) -> a -> Log m b -> m ()
logSeverity sev f a logger = f sev a `logTo` logger

trace :: (Severity -> a -> b) -> a -> Log m b -> m ()
trace = logSeverity Trace

debug :: (Severity -> a -> b) -> a -> Log m b -> m ()
debug = logSeverity Debug

info :: (Severity -> a -> b) -> a -> Log m b -> m ()
info = logSeverity Info

warn :: (Severity -> a -> b) -> a -> Log m b -> m ()
warn = logSeverity Warning

error :: (Severity -> a -> b) -> a -> Log m b -> m ()
error = logSeverity Error

critical :: (Severity -> a -> b) -> a -> Log m b -> m ()
critical = logSeverity Critical
