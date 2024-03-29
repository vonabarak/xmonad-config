module Config ( main ) where
--
import Control.Concurrent ( threadDelay )
import Data.Monoid
import GHC.IO.Handle.Types ( Handle )
import System.IO ( hPutStrLn )
import XMonad
import XMonad.Actions.Promote
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.PerWindowKbdLayout
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch ( fuzzyMatch )
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Cursor ( setDefaultCursor )
import XMonad.Util.Run ( spawnPipe )

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import ManageHook ( myManageHook )
import Keys ( myKeys )
import Runner ( respawn )
import Layout ( myLayout )
import XMonad.Actions.CycleWS


-- Terminal programm
myTerminal      = "alacritty"

-- Font

myFont = "xft:terminus:size=18:hinting=0:antialias=1"
mySmallFont = "xft:noto:size=10:hinting=1:antialias=1"


-- Colors
myDefaultColor  = "green"
myBgColor       = "black"
myHlColor       = "yellow"
myActiveColor   = "red"
myInactiveColor = "grey"
myUrgentColor   = "blue"

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor   = "grey"
myFocusedBorderColor  = "red"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
-- For automatic hotkeys generator to work workspace name must be a one
-- printable character
myWorkspaces :: [String]
myWorkspaces    = [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"
                  , "q", "w", "e", "r", "t", "y", "u", "i", "o", "p"
                  ]

myTabConfig :: Theme
myTabConfig = def
    { activeColor         = myBgColor
    , inactiveColor       = myBgColor
    , urgentColor         = myBgColor
    , activeBorderColor   = myFocusedBorderColor
    , inactiveBorderColor = myNormalBorderColor
    , urgentBorderColor   = myNormalBorderColor
    , activeBorderWidth   = 1
    , inactiveBorderWidth = 1
    , urgentBorderWidth   = 1
    , activeTextColor     = myActiveColor
    , inactiveTextColor   = myDefaultColor
    , urgentTextColor     = myUrgentColor
    , decoHeight          = 20
    , fontName            = mySmallFont
    }

myXPConfig :: XPConfig
myXPConfig = def
    { font                = myFont
    , bgColor             = myBgColor
    , fgColor             = myDefaultColor
    , bgHLight            = myBgColor
    , fgHLight            = myFocusedBorderColor
    , borderColor         = myNormalBorderColor
    , promptBorderWidth   = 1
    , height              = 32
    , position            = Top
    , historySize         = 100000
    , historyFilter       = deleteConsecutive
    , searchPredicate     = fuzzyMatch
--    , autoComplete        = Nothing
    }


-- Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)
    , ((modm, button5), const nextWS)
    , ((modm, button4), const prevWS)
    ]


-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook :: Event -> X All
myEventHook event = do
     -- commands by name
    _ <- serverModeEventHookCmd event

    -- commands by index
    -- serverModeEventHook event

    -- extra commands may be defined here
    -- here's an example of command that just prints a string to 
    -- xmonad's stderr with xmonadctl -a XMONAD_PRINT hello
    _ <- serverModeEventHookF "XMONAD_PRINT" (io . putStrLn) $ event
    -- or shows a notification
    _ <- serverModeEventHookF "XMONAD_NOTIFY" (\s -> spawn ("notify-send " ++ s)) $ event
    perWindowKbdLayout event

-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
myLogHook :: Handle -> X ()
myLogHook h = clickablePP (myXmobarPP h) >>= dynamicLogWithPP

-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-shift-z. Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook :: X ()
myStartupHook = do
              setDefaultCursor xC_left_ptr
              setWMName "LG3D"
              respawn "picom"

myXmobarPP :: Handle -> PP
myXmobarPP h = xmobarPP
           { ppOutput = hPutStrLn h
           --, ppCurrent = xmobarColor myActiveColor myBgColor . wrap "[" "]"
           , ppCurrent = xmobarColor myActiveColor myBgColor
           --, ppHiddenNoWindows = xmobarColor myInactiveColor myBgColor
           , ppTitle = xmobarColor myHlColor myBgColor . shorten 160
           , ppSep = xmobarColor myInactiveColor myBgColor " | "
           , ppLayout = layoutSwitcher . shorten 9
           }
        where
            layoutSwitcher text = "<action=`xmonadctl next-layout`>" ++ text ++ "</action>"

-- Run xmonad with the settings you specify.
main :: IO ()
main = do
    -- delay 2 sec for plasma to start
    threadDelay 2000000
    hXmobar <- spawnPipe "xmobar"
    xmonad $ ewmh $ docks $ def {
          terminal           = myTerminal,
          focusFollowsMouse  = myFocusFollowsMouse,
          borderWidth        = myBorderWidth,
          modMask            = myModMask,
          workspaces         = myWorkspaces,
          normalBorderColor  = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,

          keys               = myKeys myXPConfig,
          mouseBindings      = myMouseBindings,

          layoutHook         = myLayout myTabConfig,
          -- insertPosition :: Position -> Focus -> ManageHook
          -- Position: Master End Above Below
          -- Focus: Newer Older
          manageHook         = insertPosition Above Newer <+> myManageHook,
          handleEventHook    = myEventHook,
          startupHook        = myStartupHook,
          logHook            = myLogHook hXmobar
    }
