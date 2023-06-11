module System.Log.Contra.Severity where

import Data.Dynamic (Typeable)
import System.Log.Contra.Internal (Log, logWhen)
import System.Log.Contra.Message (Message, getValue, withCallStack, withValue, logMsg)
import GHC.Stack (HasCallStack, callStack)

data Severity
  = Trace
  | Debug
  | Info
  | Warning
  | Error
  | Critical
  deriving (Read, Eq, Ord, Show)

getSeverity :: Message -> Maybe Severity
getSeverity = getValue "@severity"

minSeverity :: (Applicative m) => Severity -> Log m Message -> Log m Message
minSeverity severity = logWhen (maybe True (>= severity) . getSeverity)

withSeverity :: Severity -> Log m Message -> Log m Message
withSeverity = withValue "@severity"

debug :: (Typeable a, HasCallStack) => Log m Message -> a -> m ()
debug = logMsg . withCallStack callStack . withSeverity Debug

trace :: (Typeable a, HasCallStack) => Log m Message -> a -> m ()
trace = logMsg . withCallStack callStack . withSeverity Trace

info :: (Typeable a, HasCallStack) => Log m Message -> a -> m ()
info = logMsg . withCallStack callStack . withSeverity Info

warn :: (Typeable a, HasCallStack) => Log m Message -> a -> m ()
warn = logMsg . withCallStack callStack . withSeverity Warning

err :: (Typeable a, HasCallStack) => Log m Message -> a -> m ()
err = logMsg . withCallStack callStack . withSeverity Error

critical :: (Typeable a, HasCallStack) => Log m Message -> a -> m ()
critical = logMsg . withCallStack callStack . withSeverity Critical
