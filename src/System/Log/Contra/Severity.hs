module System.Log.Contra.Severity where

import Data.Dynamic (Typeable, toDyn)
import Data.Functor.Contravariant (contramap)
import qualified Data.HashMap.Strict as Map
import System.Log.Contra.Internal (Log, logTo, logWhen)
import System.Log.Contra.Message (Message, getValue, setValue)

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

setSeverity :: Severity -> Message -> Message
setSeverity = setValue "@severity"

minSeverity :: (Applicative m) => Severity -> Log m Message -> Log m Message
minSeverity severity = logWhen (maybe True (>= severity) . getSeverity)

logWithSeverity :: (Typeable a) => Severity -> Log m Message -> a -> m ()
logWithSeverity severity logger =
  logTo (contramap (setSeverity severity) logger)
    . Map.singleton "@message"
    . toDyn

debug :: (Typeable a) => Log m Message -> a -> m ()
debug = logWithSeverity Debug

trace :: (Typeable a) => Log m Message -> a -> m ()
trace = logWithSeverity Trace

info :: (Typeable a) => Log m Message -> a -> m ()
info = logWithSeverity Info

warn :: (Typeable a) => Log m Message -> a -> m ()
warn = logWithSeverity Warning

err :: (Typeable a) => Log m Message -> a -> m ()
err = logWithSeverity Error

critical :: (Typeable a) => Log m Message -> a -> m ()
critical = logWithSeverity Critical
