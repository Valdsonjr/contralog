module Main (main) where

import System.Log.Contra (Log (Log), fallbackTo, logTo, logWhen, nowhere)
import Test.Hspec (describe, hspec, it)

main :: IO ()
main = hspec $ do
  describe "Contralog.fallbackTo" $
    it "fallsback to second handler" $
      logTo (exceptionLogHandler `fallbackTo` nowhere) "teste"
  describe "Contralog.logWhen" $
    it "filters in logs" $
      logTo (logWhen (const False) exceptionLogHandler) "teste"

exceptionLogHandler :: Log IO a
exceptionLogHandler = Log (const $ ioError $ userError "nope")
