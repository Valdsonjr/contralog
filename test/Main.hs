module Main (main) where

import Control.Concurrent (myThreadId)
import Data.IORef (IORef, newIORef, readIORef)
import System.Log.Contra
  ( Log (Log),
    Severity (..),
    contramapM,
    critical,
    debug,
    error,
    fallbackTo,
    info,
    ioRef,
    logTo,
    logUnless,
    logWhen,
    nowhere,
    trace,
    warn,
  )
import Test.Hspec (describe, hspec, it, shouldStartWith)
import Text.Printf (printf)
import Prelude hiding (error)

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
  describe "Severity" $ do
    it "trace" $ do
      ref <- newIORef ""
      trace Simple "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Trace]"
    it "debug" $ do
      ref <- newIORef ""
      debug Simple "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Debug]"
    it "info" $ do
      ref <- newIORef ""
      info Simple "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Info]"
    it "warn" $ do
      ref <- newIORef ""
      warn Simple "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Warning]"
    it "error" $ do
      ref <- newIORef ""
      error Simple "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Error]"
    it "critical" $ do
      ref <- newIORef ""
      critical Simple "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Critical]"

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