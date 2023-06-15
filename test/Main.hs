module Main (main) where

import Control.Concurrent (myThreadId)
import Data.IORef (IORef, newIORef, readIORef)
import System.Log.Contra
  ( Log (Log),
    Severity (..),
    contramapM,
    fallbackTo,
    ioRef,
    logTo,
    logUnless,
    logWhen,
    nowhere,
  )
import Test.Hspec (describe, hspec, it, shouldNotBe)
import Text.Printf (printf)

main :: IO ()
main = hspec $ do
  describe "Contra.fallbackTo" $
    it "falls back to second handler" $
      "test" `logTo` (exceptionLogger `fallbackTo` nowhere)
  describe "Contra.logWhen" $
    it "filters in logs" $
      "test" `logTo` logWhen (const False) exceptionLogger
  describe "Contra.logUnless" $
    it "filters out logs" $
      "test" `logTo` logUnless (const True) exceptionLogger
  describe "something-something" $
    it "works" $ do
      ref <- newIORef ""
      Simple Trace "teste" `logTo` ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldNotBe` ""

exceptionLogger :: Log IO a
exceptionLogger = Log (const $ ioError $ userError "nope")

data Message
  = Simple Severity String
  | Complex Severity Int Int Bool

ioRefMessageLogger :: IORef String -> Log IO Message
ioRefMessageLogger = contramapM fmt . ioRef
  where
    fmt (Simple sev msg) = do
      threadId <- myThreadId
      pure $ printf "[%s] [%s] %s" (show sev) (show threadId) msg
    fmt (Complex sev i1 i2 b) =
      pure $ printf "[%s] %d %d %s" (show sev) i1 i2 (show b)