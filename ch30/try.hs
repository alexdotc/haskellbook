module Main where

import Control.Exception
import System.Environment (getArgs)

onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
  result <- action
  case result of
    Left e -> print e
    Right _ -> return ()

willFail :: Integer -> IO ()
willFail denom = onlyReportError $ willIFail denom

willIFail' :: Integer -> IO ()
willIFail' denom = print (div 5 denom) `catch` handler
  where handler :: ArithException -> IO ()
        handler e = print e

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

testDiv :: String -> IO ()
testDiv d = onlyReportError $ willIFail (read d)

main :: IO ()
main = do
  args <- getArgs
  mapM_ testDiv args