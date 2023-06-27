module Main (main) where

import Control.Concurrent (myThreadId)
import Data.IORef (IORef, newIORef, readIORef)
import System.Log.Contra
  ( Log (Log),
    WithSeverity,
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
    severity,
    trace,
    value,
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
      trace "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Trace]"
    it "debug" $ do
      ref <- newIORef ""
      debug "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Debug]"
    it "info" $ do
      ref <- newIORef ""
      info "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Info]"
    it "warn" $ do
      ref <- newIORef ""
      warn "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Warning]"
    it "error" $ do
      ref <- newIORef ""
      error "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Error]"
    it "critical" $ do
      ref <- newIORef ""
      critical "teste" $ ioRefMessageLogger ref
      msg <- readIORef ref
      msg `shouldStartWith` "[Critical]"

exceptionLogger :: Log IO a
exceptionLogger = Log (const $ ioError $ userError "nope")

ioRefMessageLogger :: IORef String -> Log IO (WithSeverity String)
ioRefMessageLogger = contramapM fmt . ioRef
  where
    fmt :: WithSeverity String -> IO String
    fmt entry = do
      threadId <- myThreadId
      pure $
        printf
          "[%s] [%s] %s"
          (show $ severity entry)
          (show threadId)
          (value entry)
