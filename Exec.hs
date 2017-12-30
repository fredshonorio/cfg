module Exec (run, runIsSuccess, runReadShell, runReadProc, onlyWhen) where

import System.Process.Typed
import GHC.IO.Exception (ExitCode(..))
import Data.Functor
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T

-- https://haskell-lang.org/library/typed-process

-- Run a process (no shell) without a meaningful value
run :: String -> [String] -> IO ()
run prog args =
  void $ runProcess_ (proc prog args)

-- Run a process (no shell) and return wether it exited successfully
runIsSuccess :: String -> [String] -> IO Bool
runIsSuccess prog args = do
  (retCode, _, _) <- readProcess $ proc prog args -- swallow output, capture return code
  return (retCode == ExitSuccess)

-- Run a shell command and capture output
runReadShell :: String -> IO T.Text
runReadShell cmd =
  do
    (out, _) <- readProcess_ $ shell cmd
    return $ T.pack $ C.unpack out

runReadProc :: String -> [String] -> IO T.Text
runReadProc prog args =
  do
    (out, _) <- readProcess_ $ proc prog args
    return $ T.pack $ C.unpack out

onlyWhen :: Bool -> IO () -> IO ()
onlyWhen condition action = do
  if condition then action else return ()
