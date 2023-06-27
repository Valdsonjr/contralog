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
  Applicative m =>
  Severity ->
  Log m (WithSeverity a) ->
  Log m (WithSeverity a)
withMinSeverity sev = logWhen ((>= sev) . severity)

data WithSeverity a = WithSeverity {severity :: Severity, value :: a}

logSeverity :: Severity -> a -> Log m (WithSeverity a) -> m ()
logSeverity sev a logger = WithSeverity sev a `logTo` logger

trace :: a -> Log m (WithSeverity a) -> m ()
trace = logSeverity Trace

debug :: a -> Log m (WithSeverity a) -> m ()
debug = logSeverity Debug

info :: a -> Log m (WithSeverity a) -> m ()
info = logSeverity Info

warn :: a -> Log m (WithSeverity a) -> m ()
warn = logSeverity Warning

error :: a -> Log m (WithSeverity a) -> m ()
error = logSeverity Error

critical :: a -> Log m (WithSeverity a) -> m ()
critical = logSeverity Critical
