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
-- TODO: vera.merge_to_dir("~/SpiderOak Hive/ssh", "~/.ssh/", force)
-- TODO: ensure line in file
-- TODO: ensure app unninstaled

-- Utilities that might not need to be permantly installed
-- soundconverter -> converts audio files between formats

main :: IO ()
main = do
  force <- elem "--force" <$> getArgs
  runAll $
    [ aur   "google-chrome-beta", aur "firefox-beta-bin"
    , pac   "veracrypt"         , aur "spideroak-one"
    , aur   "spotify"
    , pac   "redshift"
    , merge "files/unpushed" "~/.bin/unpushed"
    ] ++

    -- shell
    [ aur   "powerline-fonts", aur "prezto-git"
    , merge "files/zshrc" "~/.zshrc"
    , merge "files/profile" "~/.profile"    
    , aur   "z", pac "lsof", pac "htop", aur "entr", pac "pass"

    , pac   "sakura" -- TODO: chsh -s zsh
    , merge "files/sakura.conf" "~/.config/sakura/sakura.conf"
    ] ++

    -- git
    [ pac   "git", aur "git-cola" , pac "gitg"
    , pac   "tk" , pac "aspell-en", pac "meld"
    , merge "files/gitconfig" "~/.gitconfig"
    ] ++

    -- emacs
    [ pac   "emacs"
    -- TODO: install bbastov/prelude unless ~/.emacs.d/ exists
    , merge "files/emacs_prelude_personal.el" "~/.emacs.d/personal/personal.el"
    ] ++

    -- dev
    [ pac   "jdk8-openjdk"--, pac "gradle"
    , pac   "scala"       , pac "sbt"    , aur "ammonite"
    , aur   "dbeaver"     , pac "cloc"   , aur "haroopad"
    , aur   "intellij-idea-community-edition"
    , aur   "slack-desktop"
    , merge "files/gradle.properties" "~/.gradle/gradle.properties"
    ] ++

    -- ops
    [ pac   "python2-pip", pac "python-pip"
    , aur   "fabric",      pac "aws-cli"
    , pac   "docker",      pac "docker-compose"
    , merge "files/aws_config" "~/.aws/config"
    ] ++

    -- desktop
    [ aur   "ttf-iosevka"
    , pac   "xmobar"
    , merge "files/xmobarrc" "~/.xmobarrc"

    , pac   "xmonad", pac "xmonad-contrib", aur "compton"
    , merge "files/xmonad.hs" "~/.xmonad/xmonad.hs"

    , pac   "rofi", aur "rofi-dmenu" -- themes in /usr/share/rofi/
    , merge "files/rofi_config.rasi" "~/.config/rofi/config.rasi"

    , pac   "feh"
    , pac   "trayer"
    , aur   "stlarch_icons" -- icons installed in /usr/share/icons/stlarch_ico
    ] ++

    -- apps
    [ pac "nemo"
    , pac "mpv", pac "vlc", pac "smplayer"
    , pac "android-udev"

    , aur "jdownloader2", aur "cclive", aur "youtube-dl"
    , aur "caffeine-ng"
    , aur "dukto"
    ] ++

    -- quirks
    [ forHost "liminal"
      [ aur "powertop"
      ]
    , forHost "hornet"
      [ aur "atom-editor-bin"
      ]
    ]
