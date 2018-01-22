module Vera (mergeVeracryptDir) where
import Exec
import Data.List
import qualified Data.Text as T
import Control.Exception

-- vPath, dest should be expanded
mergeVeracryptDir volumePath dest =
  bracket
    (_mount volumePath)
    (\_ -> _unmount volumePath)
    (flip _diff dest)

_diff src dst = run "meld" [src, dst]

_unmount :: FilePath -> IO ()
_unmount volumePath =
  run "veracrypt" ["--mount", volumePath]

_mount :: FilePath -> IO FilePath
_mount volumePath =
  do
    run "veracrypt" ["--mount", volumePath]
    out <- runReadProc "veracrypt" ["-t", "--volume-properties", volumePath]
    T.unpack <$> _getVolumeMountPoint out

mountPrefix = T.pack "Mount Directory: "

($>) = flip ($)

_getVolumeMountPoint :: T.Text -> IO T.Text
_getVolumeMountPoint out =
  let line = out $> T.lines $> filter (T.isPrefixOf mountPrefix) $> _head
  in T.replace mountPrefix (T.pack "") <$> line

_head :: [T.Text] -> IO T.Text
_head xs =
  let hd = fst <$> uncons xs
  in _failMaybe "Can't find mount point for volume" hd

_failMaybe :: String -> Maybe a -> IO a
_failMaybe msg m =
  case m of
    Nothing -> fail msg
    Just a -> return a

output = T.pack "Slot: 5\n\
  \Volume: /home/fred/backup_local/18.vc\n\
  \Virtual Device: /dev/mapper/veracrypt5\n\
  \Mount Directory: /mnt/veracrypt5\n\
  \Size: 18,0 GB\n\
  \Type: Normal\n\
  \Read-Only: No\n\
  \Hidden Volume Protected: No\n\
  \Encryption Algorithm: AES\n\
  \Primary Key Size: 256 bits\n\
  \Secondary Key Size (XTS Mode): 256 bits\n\
  \Block Size: 128 bits\n\
  \Mode of Operation: XTS\n\
  \PKCS-5 PRF: HMAC-SHA-512\n\
  \Volume Format Version: 2\n\
  \Embedded Backup Header: Yes"
