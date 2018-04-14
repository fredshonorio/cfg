module Lib (aur, pac, merge, forHost) where

import Plan (Plan, unless, flatten, when)
import System.Directory as Dir
import System.FilePath.Posix as Path
import qualified Data.Text as T
import Exec
import Vera

{-
expandUser is called in too many places
I could either make expandUser pure or make Plan a monad/make it easy to compose with IO
-}

aur :: String -> Plan
aur pkg = unless (isInstalled pkg) $
  putStrLn ("Installing " ++ pkg)
  >> run "trizen" ["-S", pkg, "--needed"]

pac :: String -> Plan
pac pkg = unless (isInstalled pkg) $
  putStrLn ("Installing " ++ pkg)
  >> run "sudo" ["pacman", "-S", pkg, "--needed"]

mergeTool :: String
mergeTool = "meld"

expandUser :: FilePath -> IO FilePath
expandUser pth =
  let cmd = "echo " ++ pth
  in T.unpack . T.strip <$> runReadShell cmd

merge :: FilePath -> FilePath -> Plan
merge src dst = unless (filesAreEqual src dst) $
  do
    expandedDst <- expandUser dst
    run "mkdir" ["-p", Path.takeDirectory expandedDst ] -- create directory or no-op
    exists <- isFile expandedDst
    onlyWhen (not exists) $ run "touch" [expandedDst]
    run mergeTool [src, expandedDst]

forHost :: String -> [Plan] -> Plan
forHost hostName plans = when ((==) hostName <$> host) $
  flatten plans

host :: IO String
host = T.unpack .T.strip <$> runReadShell "hostname"

{-
_mergeEncDir :: FilePath -> FilePath -> IO ()
mergeEncryptedDir :: FilePath -> FilePath -> Bool -> Plan
mergeEncryptedDir = _
-}

isFile :: String -> IO Bool
isFile = Dir.doesFileExist

isInstalled :: String -> IO Bool
isInstalled pkg =
  runIsSuccess "pacman" ["-Q", pkg]

filesAreEqual :: String -> FilePath -> IO Bool
filesAreEqual a b =
  do
    expandedB <- expandUser b
    runIsSuccess "diff" [a, expandedB]
