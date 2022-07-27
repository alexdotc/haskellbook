module Main where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  putStrLn ("Running main caused an error! It was: " ++ show e)
  writeFile "bbb" "hi"

canICatch :: Exception e => e -> IO (Either ArithException ())
canICatch e = try $ throwIO e

main = forever 

main = writeFile "zzz" "hi" `catch` handler
