#!/usr/bin/env stack
{- stack script --resolver lts-10.1 --install-ghc
  --package typed-process --package directory --package filepath --package bytestring --package text
-}
-- https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter
-- run: ./install.hs

{-# LANGUAGE OverloadedStrings #-}
import Lib
import Plan
import System.Environment
import Xfce
-- TODO: vera.merge_to_dir("~/SpiderOak Hive/ssh", "~/.ssh/", force)
-- TODO: ensure line in file
-- TODO: ensure app unninstaled
-- TODO: chsh -s zsh, after installing zsh
-- TODO: install bbastov/prelude unless ~/.emacs.d/ exists
-- TODO: modprobe blacklist
-- TODO: emoji support https://www.reddit.com/r/archlinux/comments/52k3t0/proper_color_emoji_support/d7l05nt/

-- Utilities that might not need to be permantly installed
-- soundconverter -> converts audio files between formats

main :: IO ()
main = do
  force <- elem "--force" <$> getArgs
  runAll $
    [ aur   "google-chrome-beta", aur "firefox-beta-bin", aur "min-browser-bin"
    , aur   "spotify", pac "redshift"
    , merge "files/unpushed" "~/.bin/unpushed"
    ] ++

    -- backup
    [ pac   "veracrypt", aur "securefs", pac "pass" ] ++

    -- shell
    [ aur   "powerline-fonts", aur "prezto-git"
    , merge "files/zshrc" "~/.zshrc"
    , merge "files/profile" "~/.profile"    
    , aur   "z", pac "lsof", pac "htop", aur "entr"
    , pac   "bat" -- improved `cat`

    , pac   "sakura"
    , merge "files/sakura.conf" "~/.config/sakura/sakura.conf"
    ] ++

    -- git
    [ pac   "git", aur "git-cola", pac "gitg"
    , pac   "tk" , pac "aspell-en", pac "meld"
    , merge "files/gitconfig" "~/.gitconfig"
    ] ++

    -- emacs
    [ pac   "emacs", merge "files/emacs_prelude_personal.el" "~/.emacs.d/personal/personal.el" ] ++

    -- dev
    [ pac   "jdk8-openjdk"
    , pac   "scala"       , pac "sbt"    , aur "ammonite"
    , aur   "dbeaver"     , pac "cloc"
    , aur   "intellij-idea-community-edition"
    , aur   "slack-desktop"
    , merge "files/gradle.properties" "~/.gradle/gradle.properties" -- TODO: use securefs for secret stuff, mount and have a withSecureFs :: FileName -> (FileName -> IO a) -> IO a that mounts a partition in /tmp and runs the function passig the tmp dir
    ] ++

    -- ops
    [ pac   "python2-pip"
    , pac   "aws-cli"    , aur "aws-vault"
    , pac   "docker",      pac "docker-compose"
    , cmd   "sudo pip2 install fabric==1.13.1"
    , merge "files/aws_config" "~/.aws/config"
    ] ++

    -- desktop
    [ aur   "ttf-iosevka"
    , pac   "xmobar"
    , merge "files/xmobarrc" "~/.xmobarrc"

    , pac   "xmonad", pac "xmonad-contrib"
    , merge "files/xmonad.hs" "~/.xmonad/xmonad.hs"

    , pac   "rofi", aur "rofi-dmenu" -- themes in /usr/share/rofi/
    , merge "files/rofi_config.rasi" "~/.config/rofi/config.rasi"

    , pac   "feh"
    , pac   "trayer"
    , aur   "stlarch_icons" -- icons installed in /usr/share/icons/stlarch_ico
    ] ++

    -- ssd
    [ pac   "util-linux", cmd "sudo systemctl enable fstrim.timer" ] ++

    -- apps
    [ pac "nemo",  pac "vlc", pac "smplayer"
    , pac "android-udev"

    , aur "jdownloader2", aur "cclive", aur "youtube-dl"
    , aur "caffeine-ng"
    , aur "dukto"
    ] ++

    -- quirks
    [ forHost "liminal"
      [ aur "powertop" ]

    , forHost "hornet"
      [ aur "atom-editor-bin" ]
    ] ++

    -- disable keyboard shortcuts
    [ unsetKbShortcut "/commands/custom/<Alt>F7"     -- usually "move_window_key"  , conflicts with intellij
    , unsetKbShortcut "/commands/custom/<Alt>F6"     -- usually "stick_window_key" , conflicts with intellij
    , unsetKbShortcut "/commands/custom/<Alt>Insert" -- usually "add_workspace_key", conflicts with intellij
    ]
