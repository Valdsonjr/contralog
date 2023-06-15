module System.Log.Contra.Severity where

import System.Log.Contra.Internal (Log, logWhen)

data Severity
  = Trace
  | Debug
  | Info
  | Warning
  | Error
  | Critical
  deriving (Read, Eq, Ord, Show)

withMinSeverity :: (Applicative m) 
  => Severity 
  -> (a -> Severity) 
  -> Log m a 
  -> Log m a
withMinSeverity severity getter = logWhen ((>= severity) . getter)

