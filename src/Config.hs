module Config ( main ) where
--
import Control.Concurrent ( threadDelay )
import Data.Monoid
import GHC.IO.Handle.Types ( Handle )
import System.IO ( hPutStrLn )
import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.Fullscreen
import XMonad.Layout.GridVariants
import XMonad.Layout.LayoutModifier ( ModifiedLayout )
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Cursor ( setDefaultCursor )
import XMonad.Util.Run ( spawnPipe )

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import ManageHook ( myManageHook )
import Keys ( myHotKeys )
import Runner ( respawn )



-- Terminal programm
myTerminal      = "alacritty"

-- Font
myFont = "xft:terminus:size=18:hinting=0:antialias=1"
mySmallFont = "xft:noto:size=10:hinting=1:antialias=1"
-- mySmallFont = "xft:terminus:size=10:hinting=0:antialias=1"

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
--
-- A tagging example:
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
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
--    , autoComplete        = Nothing
    }




myLayout = onWorkspace "1" workspace1 $
           onWorkspace "0" workspace0 $
           onWorkspace "w" workspaceW $
           onWorkspace "e" workspaceE $
           onWorkspace "r" workspaceR $
           allOthers
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 70/100
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    tiled'  = avoidStruts $ borders tiled
    grid    = avoidStruts $ borders $ Grid (16/9)
    tabbed' = avoidStruts $ noBorders $ tabbed shrinkText myTabConfig
    full    = avoidStruts $ noBorders Full
    
    borders :: LayoutClass l a => l a -> ModifiedLayout (ConfigurableBorder Ambiguity) l a
    borders = lessBorders Screen
    --  borders = lessBorders (Combine Union Screen OnlyFloat)

    workspace1 = full
    workspace0 = noBorders $ fullscreenFull Full
    workspaceW = tabbed' ||| tiled' ||| grid   ||| full
    workspaceE = tabbed' ||| tiled' ||| grid   ||| full
    workspaceR = tabbed' ||| tiled' ||| grid   ||| full
    allOthers  = tiled'  ||| grid   ||| tabbed'||| full


-- Key bindings. Add, modify or remove key bindings here.xmessage
-- the list of kesyms may be found at /usr/include/X11/keysymdef.h
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    myHotKeys conf myXPConfig

    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [
        xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0,
        xK_q, xK_w, xK_e, xK_r, xK_t, xK_y, xK_u, xK_i, xK_o, xK_p
        ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    --mod-{a,s,d}, Switch to physical/Xinerama screens 1, 2, or 3
    --mod-shift-{a,s,d}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w 
                                      >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook :: Event -> X All
myEventHook event = do
     -- commands by name
    serverModeEventHookCmd event

    -- commands by index
    -- serverModeEventHook event

    -- extra commands may be defined here
    -- here's an example of command that just prints a string to 
    -- xmonad's stderr with xmonadctl -a XMONAD_PRINT hello
    serverModeEventHookF "XMONAD_PRINT" (io . putStrLn) $ event
    -- or shows a notification
    serverModeEventHookF "XMONAD_NOTIFY" (\s -> spawn ("notify-send " ++ s)) $ event

-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
-- myLogHook = return()

-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-shift-z. Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook :: X ()
myStartupHook = do
              setDefaultCursor xC_left_ptr
              setWMName "LG3D"
              respawn "conky"
              respawn "picom"

myXmobarPP :: Handle -> PP
myXmobarPP h = xmobarPP
           { ppOutput = hPutStrLn h
           --, ppCurrent = xmobarColor myActiveColor myBgColor . wrap "[" "]"
           , ppCurrent = xmobarColor myActiveColor myBgColor
           --, ppHiddenNoWindows = xmobarColor myInactiveColor myBgColor
           , ppTitle = xmobarColor myHlColor myBgColor . shorten 160
           , ppSep = xmobarColor myInactiveColor myBgColor " | "
           , ppLayout = (\s -> "<action=`xmonadctl next-layout`>" ++ s ++ "</action>") . shorten 30
           }

-- Run xmonad with the settings you specify.
main :: IO ()
main = do
    -- delay 2 sec for plasma to start
    threadDelay 2000000
    xmobar <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ ewmh $ docks $ kde4Config {
          terminal           = myTerminal,
          focusFollowsMouse  = myFocusFollowsMouse,
          borderWidth        = myBorderWidth,
          modMask            = myModMask,
          workspaces         = myWorkspaces,
          normalBorderColor  = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,

          keys               = myKeys,
          mouseBindings      = myMouseBindings,

          layoutHook         = myLayout,
          -- insertPosition :: Position -> Focus -> ManageHook
          -- Position: Master End Above Below
          -- Focus: Newer Older
          manageHook         = insertPosition Above Newer <+> myManageHook,
          handleEventHook    = myEventHook,
          startupHook        = myStartupHook,
          logHook = clickablePP (myXmobarPP xmobar) >>= dynamicLogWithPP
    }

