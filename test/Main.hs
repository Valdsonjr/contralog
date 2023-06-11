module Main (main) where

import System.Log.Contra
  ( Log (Log),
    fallbackTo,
    logTo,
    logUnless,
    logWhen,
    nowhere,
  )
import Test.Hspec (describe, hspec, it)

main :: IO ()
main = hspec $ do
  describe "Contralog.fallbackTo" $
    it "falls back to second handler" $
      logTo (exceptionLogHandler `fallbackTo` nowhere) "test"
  describe "Contralog.logWhen" $
    it "filters in logs" $
      logTo (logWhen (const False) exceptionLogHandler) "test"
  describe "Contralog.logUnless" $
    it "filters out logs" $
      logTo (logUnless (const True) exceptionLogHandler) "test"

exceptionLogHandler :: Log IO a
exceptionLogHandler = Log (const $ ioError $ userError "nope")
