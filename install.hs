#!/usr/bin/env stack
{- stack script --resolver lts-10.1 --install-ghc
  --package typed-process --package directory --package filepath --package bytestring --package text
-}
-- https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter

{-
typecheck: $ stack ghc install.hs -- -Wall
repl:      $ stack ghci install.hs
run:       $ ./install.hs
-}

{-# LANGUAGE OverloadedStrings #-}
import Lib
import Plan
import System.Environment
-- # bootstrap
-- pacman -S yaourt && yaourt -S stack-bin

-- add quirks based on hostname

main :: IO ()
main = do
  force <- elem "--force" <$> getArgs
  runAll $
    [ pac   "yaourt"
    , aur   "google-chrome-beta", aur "firefox-beta-bin"
    , pac   "veracrypt"         , aur "spideroak-one"
    , aur   "spotify"
    , pac   "redshift"
    , pac   "cloc"
    , aur   "haroopad"
    , merge "files/unpushed" "~/.bin/unpushed"
    ] ++ -- shell
    [ aur   "powerline-fonts"
    , aur   "prezto-git"
    , merge "files/zshrc" "~/.zshrc"
    , merge "files/profile" "~/.profile"    
    , aur   "terminator" -- deprecated
    , merge "files/terminator_config" "~/.config/terminator/config" -- deprecated
    ] ++ -- git
    [ pac   "git",       pac "tk"
    , pac   "aspell-en", pac "gitg", pac "meld"
    , merge "files/gitconfig" "~/.gitconfig"
    ] ++ -- emacs
    [ pac   "emacs"
    -- TODO: install bbastov/prelude unless ~/.emacs.d/ exists
    , merge "files/emacs_prelude_personal.el" "~/.emacs.d/personal/personal.el"
    ] ++ -- dev
    [ pac   "jdk8-openjdk"
    , aur   "intellij-idea-community-edition"
    , aur   "dbeaver"
    , merge "files/gradle.properties" "~/.gradle/gradle.properties"
    ] ++ -- ops
    [ pac   "python2-pip", pac "python-pip"
    , aur   "fabric",      pac "aws-cli"
    , pac   "docker",      pac "docker-compose"
    , merge "files/aws_config" "~/.aws/config"
    ] ++ -- desktop
    [ aur "ttf-iosevka"
    , pac "xmobar"
    , merge "files/xmobarrc" "~/.xmobarrc"

    , pac "xmonad", pac "xmonad-contrib"
    , merge "files/xmonad.hs" "~/.xmonad/xmonad.hs"

    , pac "sakura"
    , merge "files/sakura.conf" "~/.config/sakura/sakura.conf"

    , pac "rofi"
    , merge "files/rofi_config.rasi" "~/.config/rofi/config.rasi"

    , pac "feh"
    , pac "trayer" -- TODO: try networkmanager-dmenu instead of trayer
    , aur "stlarch_icons" -- icons installed in /usr/share/icons/stlarch_ico

    ] ++ -- misc
    [ pac "lsof", pac "htop"
    , pac "nemo"
    , pac "mpv"
    , pac "vlc"
    , pac "smplayer"
    , pac "android-udev"
    , pac "pass"

    , aur "jdownloader2"
    , aur "cclive"
    , aur "youtube-dl"
    , aur "git-cola"
    ]

-- vera.merge_to_dir("~/SpiderOak Hive/ssh", "~/.ssh/", force)

-- in laptop
--- jdownloader2
--- caffeine-ng
--- dukto
--- stream2chromecast
--- powertop

-- in workstation
--- atom-editor-bin*
--- slack-desktop

-- optional?
--- soundconverter*
