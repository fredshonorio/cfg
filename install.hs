#!/usr/bin/env stack
{- stack script --resolver lts-10.1 --install-ghc
  --package typed-process --package directory --package filepath --package bytestring --package text
-}
-- https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter
-- run: ./install.hs

{-# LANGUAGE OverloadedStrings #-}
import Lib
import Exec
import Plan
import System.Environment
import Xfce
import Data.Functor
import Control.Arrow
import qualified Data.Text as T
-- TODO: vera.merge_to_dir("~/SpiderOak Hive/ssh", "~/.ssh/", force)
-- TODO: install bbastov/prelude unless ~/.emacs.d/ exists
-- TOOD: stop using pass, use files in a securefs mount
-- TODO: use securefs for secret stuff, mount and have a withSecureFs :: FileName -> (FileName -> IO a) -> IO a that mounts a partition in /tmp and runs the function passig the tmp dir
--- gradle.properties
--- .ssh

-- Utilities that might not need to be permantly installed
-- soundconverter -> converts audio files between formats

currentShellIs :: T.Text -> IO Bool
currentShellIs sh = (T.strip >>> (==) sh) <$> runReadShell "getent passwd $LOGNAME | cut -d: -f7"

pip2Installed :: String -> IO Bool
pip2Installed pkg = runShellIsSuccess $ "pip2 show " ++ pkg

lineIn :: String -> String -> IO Bool
lineIn file part = (lines >>> any (== part)) <$> readFile file

fileExists :: String -> IO Bool
fileExists file = runShellIsSuccess $ "test -e " ++ file

l_blacklist = "/etc/modprobe.d/blacklist-fred.conf"
f_blacklist = "files/modprobe_blacklist.conf"

main :: IO ()
main = do
  force <- elem "--force" <$> getArgs
  runAll $
    [ aur_  [ "google-chrome-beta", "firefox-beta-bin", "min-browser-bin", "spotify" ]
    , pac   "redshift"
    , merge "files/unpushed" "~/.bin/unpushed"

    -- backup
    , pac_  [ "veracrypt", "pass" ] , aur "securefs"

    -- shell
    , pac_  [ "zsh", "bat", "lsof", "htop", "sakura" ]
    , aur_  [ "powerline-fonts", "prezto-git", "z", "entr" ]
    , merge "files/zshrc" "~/.zshrc"
    , merge "files/profile" "~/.profile"    
    , merge "files/sakura.conf" "~/.config/sakura/sakura.conf"
    , whenNot (currentShellIs "/bin/zsh") $ cmd "chsh -s /bin/zsh"

    -- modprobe
    , whenNot (fileExists l_blacklist)
      $ cmd $ "sudo install -m 644 -D " ++ f_blacklist ++ " "  ++ l_blacklist -- create file and dir tree with permissions
    , whenNot (runShellIsSuccess $ "diff " ++ l_blacklist ++ " " ++ f_blacklist)
      $ cmd $ "gksu meld " ++ l_blacklist ++ " " ++  f_blacklist

    -- git
    , pac_  [ "git", "gitg", "tk", "aspell-en", "meld" ]
    , aur   "git-cola"
    , merge "files/gitconfig" "~/.gitconfig"

    -- emacs
    , pac   "emacs"
    , merge "files/emacs_prelude_personal.el" "~/.emacs.d/personal/personal.el"
    , merge "files/emacs_prelude_custom.el" "~/.emacs.d/personal/custom.el"

    -- dev
    , pac_  [ "jdk8-openjdk", "scala", "sbt", "cloc" ]
    , aur_  [ "ammonite", "dbeaver", "intellij-idea-community-edition", "slack-desktop" ]
    , merge "files/gradle.properties" "~/.gradle/gradle.properties"

    -- ops
    , pac_  [ "python2-pip", "aws-cli", "docker", "docker-compose" ]
    , aur   "aws-vault"
    , whenNot (pip2Installed "Fabric") $ cmd "sudo pip2 install fabric==1.13.1"
    , merge "files/aws_config" "~/.aws/config"

    -- desktop
    , pac_  [ "xmonad", "xmonad-contrib", "xmobar", "rofi", "feh", "trayer" ]
    , aur_  [ "ttf-iosevka"
            , "noto-fonts-emoji"
            , "stlarch_icons" -- icons installed in /usr/share/icons/stlarch_ico
            , "rofi-dmenu" -- themes in /usr/share/rofi/
            ]
    , merge "files/xmobarrc" "~/.xmobarrc"
    , merge "files/xmonad.hs" "~/.xmonad/xmonad.hs"
    , merge "files/rofi_config.rasi" "~/.config/rofi/config.rasi"
    , merge "files/fontconfig_emoji.conf" "~/.config/fontconfig/conf.d/01-emoji.conf"

    -- ssd
    , pac   "util-linux"
    , whenNot (runShellIsSuccess "systemctl is-enabled fstrim.timer | grep enabled")
      $ cmd "sudo systemctl enable fstrim.timer"

    -- apps
    , pac_  [ "nemo", "vlc", "smplayer", "android-udev" ]
    , aur_  [ "jdownloader2", "cclive", "youtube-dl", "caffeine-ng", "dukto" ]

    -- quirks
    , forHost "liminal" [ aur "powertop" ]

    -- disable keyboard shortcuts
    , unsetKbShortcut "/commands/custom/<Alt>F7"     -- move_window_key  , conflicts with intellij
    , unsetKbShortcut "/commands/custom/<Alt>F6"     -- stick_window_key , conflicts with intellij
    , unsetKbShortcut "/commands/custom/<Alt>Insert" -- add_workspace_key, conflicts with intellij
    ]
