module System.Log.Contra.Internal
  ( Log (Log),
    logTo,
    contramapM,
    merge,
    choose,
    fallbackTo,
    nowhere,
    file,
    console,
    logWhen,
    logUnless,
    logWhenM,
    logUnlessM,
    mergeM,
    hoist,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (unless, when, (<=<))
import Data.Functor.Contravariant (Contravariant (contramap))

newtype Log m a = Log {logTo :: a -> m ()}

instance Contravariant (Log m) where
  contramap f (Log g) = Log (g . f)

contramapM :: Monad m => (a -> m b) -> Log m b -> Log m a
contramapM f (Log g) = Log (g <=< f)

instance (Applicative m) => Semigroup (Log m a) where
  (<>) (Log f) (Log g) = Log $ \msg -> f msg *> g msg

instance (Applicative m) => Monoid (Log m a) where
  mempty = Log $ \_ -> pure ()

merge :: (Applicative m) => (a -> (b, c)) -> Log m b -> Log m c -> Log m a
merge f (Log l1) (Log l2) = Log $ \msg -> let (v1, v2) = f msg in l1 v1 *> l2 v2

mergeM :: Monad m => (a -> m (b, c)) -> Log m b -> Log m c -> Log m a
mergeM f (Log l1) (Log l2) = Log $ \msg -> do
  (v1, v2) <- f msg
  l1 v1 *> l2 v2

choose :: (a -> Either b c) -> Log m b -> Log m c -> Log m a
choose f (Log l1) (Log l2) = Log $ either l1 l2 . f

-- | Uses the 'Alternative' instance of 'm' to register a fallback in case the first handler fails
fallbackTo :: (Alternative m) => Log m a -> Log m a -> Log m a
fallbackTo (Log f) (Log g) = Log $ \msg -> f msg <|> g msg

logWhen :: (Applicative m) => (a -> Bool) -> Log m a -> Log m a
logWhen predicate (Log l) = Log $ \msg -> when (predicate msg) $ l msg

logUnless :: Applicative m => (a -> Bool) -> Log m a -> Log m a
logUnless predicate (Log l) = Log $ \msg -> unless (predicate msg) $ l msg

logWhenM :: Monad m => (a -> m Bool) -> Log m a -> Log m a
logWhenM predicate (Log l) =
  Log $ \msg -> predicate msg >>= \cond -> when cond $ l msg

logUnlessM :: Monad m => (a -> m Bool) -> Log m a -> Log m a
logUnlessM predicate (Log l) =
  Log $ \msg -> predicate msg >>= \cond -> unless cond $ l msg

nowhere :: (Applicative m) => Log m a
nowhere = mempty

file :: FilePath -> Log IO String
file = Log . appendFile

console :: Log IO String
console = Log putStrLn

hoist :: (m () -> n ()) -> Log m a -> Log n a
hoist f (Log g) = Log (f . g)
