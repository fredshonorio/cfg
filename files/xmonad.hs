import XMonad                    (XConfig(..), spawn, xmonad, (.|.), (<+>), shiftMask, modMask, mod4Mask, xK_p, xK_c, xK_q)
import XMonad.Config.Desktop     (desktopConfig)
import XMonad.Actions.SpawnOn    (spawnHere)
import XMonad.Util.Run           (spawnPipe, hPutStrLn)
import XMonad.Hooks.SetWMName    (setWMName)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.DynamicLog   (xmobarPP, xmobarColor, dynamicLogWithPP, PP(..), shorten)
import qualified Data.Map as M

-- monokai theme colors
mForeground = "#F8F8F2" 
mBackground = "#272822"
mGreen      = "#A6E22E"
mRed        = "#F92672"

main = do
  xmobarProcess <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad desktopConfig
    { terminal           = "sakura"
    , modMask            = mod4Mask
    , keys               = myKeys <+> keys desktopConfig
    , borderWidth        = 5
    , normalBorderColor  = mBackground
    , focusedBorderColor = mForeground
    , startupHook        = startup
    , logHook            = transparencyHook <+> xmobarHook xmobarProcess
    }
    
myKeys (XConfig {modMask = mod}) = M.fromList $
    [ ((mod,               xK_p), spawn "rofi -show run")
    , ((mod .|. shiftMask, xK_p), spawn "xfce4-appfinder")
    , ((mod,               xK_c), spawn "rofi -show window")
    , ((mod .|. shiftMask, xK_q), spawn "xfce4-session-logout")
    ]

wallpapers = "$HOME/SpiderOak\\ Hive/wallpapers/*" 

startup = setWMName "LG3D"                                          -- required for java apps
          >> spawnHere ("feh --randomize --bg-fill " ++ wallpapers) -- load random wallpaper
          >> spawnHere "xcompmgr"                                   -- for transparency

transparencyHook = fadeInactiveLogHook 0.94 -- percent

xmobarHook xmobarProc = dynamicLogWithPP $                 -- setup xmonad to output status to xmobar
  xmobarPP { ppOutput = hPutStrLn xmobarProc               -- write message to xmobar stdin
           , ppTitle = xmobarColor mGreen "" . shorten 140 -- display window title
           , ppCurrent = xmobarColor mRed ""               -- display current workspace
           , ppUrgent = xmobarColor mRed ""
           , ppSep = " ~ "
           }
