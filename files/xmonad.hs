import XMonad                      (XConfig(..), spawn, xmonad
                                   , (.|.), (<+>), (|||)
                                   , shiftMask, modMask, mod4Mask
                                   , xK_p, xK_c, xK_q, xK_b, xK_s, xK_f
                                   )
import XMonad.Actions.SpawnOn      (spawnHere)
import XMonad.Config.Desktop       (desktopConfig)
import XMonad.Hooks.SetWMName      (setWMName)
import XMonad.Hooks.FadeInactive   (fadeInactiveLogHook)
import XMonad.Hooks.DynamicLog     (xmobarPP, xmobarColor, dynamicLogWithPP, PP(..), shorten)
import XMonad.Hooks.ManageDocks    (avoidStruts)
import XMonad.Layout               (Full(..), Tall(..), Mirror(..))
import XMonad.Layout.Grid          (Grid(..))
import XMonad.Layout.Tabbed        (simpleTabbed)
import XMonad.Layout.ComboP        (SwapWindow(..), Property(..), combineTwoP)
import XMonad.Layout.TwoPane       (TwoPane(..))
import XMonad.Layout.Named         (named)
import XMonad.Layout.NoBorders     (noBorders)
import XMonad.Layout.Fullscreen    (fullscreenSupport)
import XMonad.Layout.Dwindle       (Dwindle(..), Chirality(..), Direction2D(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Operations           (sendMessage)
import XMonad.Util.Run             (spawnPipe, hPutStrLn)
import qualified Data.Map as M

-- monokai theme colors
mForeground = "#F8F8F2" 
mBackground = "#272822"
mGreen      = "#A6E22E"
mRed        = "#F92672"

main = do
  xmobarProcess <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $ fullscreenSupport $  desktopConfig
    { terminal           = "sakura"
    , modMask            = mod4Mask
    , keys               = myKeys <+> keys desktopConfig
    , borderWidth        = 5
    , normalBorderColor  = mBackground
    , focusedBorderColor = mForeground
    , startupHook        = startup
    , logHook            = transparencyHook <+> xmobarHook xmobarProcess
    , layoutHook         = avoidStruts layouts
    }

-- standard
tall = Tall 1 (3/100) (1/2)
-- keep two windows in view always
twoPane = TwoPane (3/100) (1/2)
-- too many terminals
grid = Grid
-- fullscreen
full = noBorders Full
-- keep non-master windows visible, use mod+shift+s to swap windows to other "pane"
streams = named "Streams" $ combineTwoP (TwoPane 0.03 0.5) simpleTabbed Grid (Const True)
-- ?
dwindle = named "Dwindle" $ Dwindle R CW 1.5 1.1

layouts = toggleLayouts full (tall ||| twoPane ||| grid ||| streams ||| dwindle)

myKeys (XConfig {modMask = mod}) = M.fromList $
    [ ((mod,               xK_p), spawn "rofi -show run")
    , ((mod .|. shiftMask, xK_p), spawn "xfce4-appfinder")
    , ((mod,               xK_c), spawn "rofi -show window")
    , ((mod .|. shiftMask, xK_q), spawn "xfce4-session-logout")
    , ((mod .|. shiftMask, xK_s), sendMessage $ SwapWindow)   -- only usable in combineTwoP (streams) layout
    , ((mod              , xK_f), sendMessage $ ToggleLayout) -- toggle fullscreen
    ]

wallpapers = "$HOME/SpiderOak\\ Hive/wallpapers/*" 

startup = setWMName "LG3D"                                          -- required for java apps
          >> spawnHere ("feh --randomize --bg-fill " ++ wallpapers) -- load random wallpaper
          >> spawnHere "xcompmgr"                                   -- for transparency

-- transparency for inactive windows
transparencyHook = fadeInactiveLogHook 0.95 -- percent

xmobarHook xmobarProc = dynamicLogWithPP $                 -- setup xmonad to output status to xmobar
  xmobarPP { ppOutput = hPutStrLn xmobarProc               -- write message to xmobar stdin
           , ppTitle = xmobarColor mGreen "" . shorten 140 -- display window title
           , ppCurrent = xmobarColor mRed ""               -- display current workspace
           , ppUrgent = xmobarColor mRed ""
           , ppSep = " ~ "
           }
