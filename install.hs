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
    [ aur_  [ "google-chrome-beta", "firefox-beta-bin", "min-browser-bin", "spotify" ]
    , pac   "redshift"
    , merge "files/unpushed" "~/.bin/unpushed"
    ] ++

    -- backup
    [ pac_  [ "veracrypt", "pass" ] , aur "securefs" ] ++

    -- shell
    [ pac_  [ "zsh", "bat", "lsof", "htop", "sakura" ]
    , aur_  [ "powerline-fonts", "prezto-git", "z", "entr" ]
    , merge "files/zshrc" "~/.zshrc"
    , merge "files/profile" "~/.profile"    
    , merge "files/sakura.conf" "~/.config/sakura/sakura.conf"
    ] ++

    -- git
    [ pac_  [ "git", "gitg", "tk", "aspell-en", "meld" ]
    , aur   "git-cola"
    , merge "files/gitconfig" "~/.gitconfig"
    ] ++

    -- emacs
    [ pac   "emacs", merge "files/emacs_prelude_personal.el" "~/.emacs.d/personal/personal.el" ] ++

    -- dev
    [ pac_  [ "jdk8-openjdk", "scala", "sbt", "cloc" ]
    , aur_  [ "ammonite", "dbeaver", "intellij-idea-community-edition", "slack-desktop" ]
    , merge "files/gradle.properties" "~/.gradle/gradle.properties" -- TODO: use securefs for secret stuff, mount and have a withSecureFs :: FileName -> (FileName -> IO a) -> IO a that mounts a partition in /tmp and runs the function passig the tmp dir
    ] ++

    -- ops
    [ pac_  [ "python2-pip", "aws-cli", "docker", "docker-compose" ]
    , aur   "aws-vault"
    , cmd   "sudo pip2 install fabric==1.13.1"
    , merge "files/aws_config" "~/.aws/config"
    ] ++

    -- desktop
    [ pac_  [ "xmonad", "xmonad-contrib", "xmobar", "rofi", "feh", "trayer" ]
    , aur_  [ "ttf-iosevka"
            , "stlarch_icons" -- icons installed in /usr/share/icons/stlarch_ico
            , "rofi-dmenu" -- themes in /usr/share/rofi/
            ]
    , merge "files/xmobarrc" "~/.xmobarrc"
    , merge "files/xmonad.hs" "~/.xmonad/xmonad.hs"
    , merge "files/rofi_config.rasi" "~/.config/rofi/config.rasi"
    ] ++

    -- ssd
    [ pac   "util-linux", cmd "sudo systemctl enable fstrim.timer" ] ++

    -- apps
    [ pac_  [ "nemo", "vlc", "smplayer", "android-udev" ]
    , aur_  [ "jdownloader2", "cclive", "youtube-dl", "caffeine-ng", "dukto" ]
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
