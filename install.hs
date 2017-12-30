#!/usr/bin/env stack
{- stack script --resolver lts-10.1 --install-ghc
  --package typed-process --package directory --package filepath --package bytestring --package text
-}
-- https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter

{-
typecheck: stack ghc install.hs -- -Wall
repl: stack ghci install.hs
run: ./install.hs
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
  force <-  elem "--force" <$> getArgs
  putStrLn "Hello World"
  runAll $
    [ pac   "yaourt"
    , aur   "google-chrome", pac "firefox"
    , pac   "veracrypt",     aur "spideroak-one"
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
    , aur   "terminator"
    , merge "files/terminator_config" "~/.config/terminator/config"
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
    ]

-- vera.merge_to_dir("~/SpiderOak Hive/ssh", "~/.ssh/", force)

-- powertop
-- stream2chromecast
-- jdownloader2
-- lsof
-- ttf-iosevka (emacs)
-- pass
-- htop
-- caffeine-ng
-- zsh
-- git-cola
-- youtube-dl
-- cclive
-- dukto
-- atom-editor-bin*
-- ttf-firacode
-- slack-desktop
-- nemo
-- mpv 
-- vlc
-- smplayer*
-- android-udev
-- soundconverter*

-- merge_encrypted_dir
