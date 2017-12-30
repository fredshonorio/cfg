module Vera (_getVolumeMountPoint, output) where
import Exec
import qualified Data.Text as T

-- mount a volume from the given path and run a computation given
-- the path where the volume is mounted
{--
withVeracryptMount :: FilePath -> (FilePath -> IO ()) -> IO ()
withVeracryptMount volumePath action = _
  
_mount volumePath =
  do
    run "veracrypt" ["--mount" volumePath]
    out <- runReadProc "veracrypt" ["-t", "--volume-properties", volumePath]
--} 

mountPrefix = T.pack "Mount Directory: "

($>) = flip ($)

_getVolumeMountPoint out =
  out
    $> T.lines
    $> filter (T.isPrefixOf mountPrefix)
    $> head -- this should be Maybe Text and then IO?
    $> T.replace mountPrefix (T.pack "")
    
{--
mergeEncryptedDir :: FilePath -> FilePath -> Bool -> Plan
mergeEncryptedDir = _
--}

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
