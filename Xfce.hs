module Xfce (unsetProp, Channel(..), unsetKbShortcut) where

import Plan (Plan, unless, when)
import Exec
import Data.Functor
import Data.List (intercalate)

data Channel = KeyboardShortcuts

chName :: Channel -> String
chName KeyboardShortcuts = "xfce4-keyboard-shortcuts"

type Path = String

unsetKbShortcut = unsetProp KeyboardShortcuts

unsetProp :: Channel -> Path -> Plan
unsetProp c p = unless (_propIsUnset c p) $
  runShell $ intercalate " " ["xfconf-query", "-c", chName c, "-p", _escape p, "-r"]

_propIsUnset :: Channel -> Path -> IO Bool
_propIsUnset c p = not <$> (runShellIsSuccess $ intercalate " " $ ["xfconf-query", "-c", chName c, "-p", _escape p, "-v" ])

_escape s = "\"" ++ s ++ "\"" -- yeah
