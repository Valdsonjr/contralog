module System.Log.Contra.Message where

import Control.Concurrent (ThreadId)
import Control.Monad ((<=<))
import Data.Dynamic (Dynamic, Typeable, dynApply, fromDynamic, toDyn)
import qualified Data.HashMap.Strict as Map
import GHC.Stack (CallStack)
import System.Log.Contra.Internal (Log, contramapM, logTo)
import Data.Functor.Contravariant (contramap)

type Message = Map.HashMap String Dynamic

getValue :: (Typeable a) => String -> Message -> Maybe a
getValue value = fromDynamic <=< Map.lookup value

setValue :: (Typeable a) => String -> a -> Message -> Message
setValue key = Map.insert key . toDyn

logMsg :: (Typeable a) => Log m Message -> a -> m ()
logMsg logger = logTo logger . Map.singleton "@message" . toDyn

withValueM :: (Monad m, Typeable a) => String -> m a -> Log m Message -> Log m Message
withValueM key action =
  contramapM
    ( \msg -> do
        value <- action
        pure $ setValue key value msg
    )

withValue :: (Typeable a) => String -> a -> Log m Message -> Log m Message
withValue key = contramap . setValue key

update :: (Typeable a) => (a -> a) -> String -> Message -> Message
update = Map.update . dynApply . toDyn

getMessage :: (Typeable a) => Message -> Maybe a
getMessage = getValue "@message"

getThreadId :: Message -> Maybe ThreadId
getThreadId = getValue "@thread_id"

withThreadId :: (Monad m) => m ThreadId -> Log m Message -> Log m Message
withThreadId = withValueM "@thread_id"

withCallStack :: CallStack -> Log m Message -> Log m Message
withCallStack = withValue "@callstack"

getCallStack :: Message -> Maybe CallStack
getCallStack = getValue "@callstack"
