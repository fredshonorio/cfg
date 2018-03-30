module Xfce where

data Channel = KeyboardShortcuts | WM

chName :: Channel -> String
chName KeyboardShortcuts = "xfce4-keyboard-shortcuts"
chName WM = "xfwm4"
