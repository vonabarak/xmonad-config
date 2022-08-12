--
import Control.Concurrent (threadDelay)
import Data.Monoid
import Data.Ratio
import GHC.IO.Handle.Types ( Handle )
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode ( serverModeEventHook )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.Fullscreen
import XMonad.Layout.GridVariants
import XMonad.Layout.LayoutHints ( layoutHints )
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise ( runOrRaisePrompt )
import XMonad.Prompt.XMonad ( xmonadPrompt )
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Cursor ( setDefaultCursor )
import XMonad.Util.Run (spawnPipe)

import qualified Language.Haskell.Interpreter  as I
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

helper          = "~/.xmonad/helper"

-- Terminal programm
myTerminal      = "konsole"

-- Font
myFont = "xft:terminus:size=18:hinting=0:antialias=1"
mySmallFont = "xft:terminus:size=10:hinting=0:antialias=1"

-- Colors
myDefaultColor  = "green"
myBgColor       = "black"
myHlColor       = "yellow"
myActiveColor   = "red"
myInactiveColor = "grey"

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "black"
myFocusedBorderColor = "red"

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
    , urgentColor         = myInactiveColor
    , activeBorderColor   = myNormalBorderColor
    , inactiveBorderColor = myNormalBorderColor
    , urgentBorderColor   = myBgColor
    , activeTextColor     = myActiveColor
    , inactiveTextColor   = myDefaultColor
    , urgentTextColor     = myInactiveColor
    , decoHeight          = 16
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

--myLayout = tiled ||| layoutHints (tabbed shrinkText myTabConfig) ||| (noBorders Full)
--    where
--        tiled   = layoutHints $ ResizableTall nmaster delta ratio []
--        tiled   = Tall nmaster delta ratio
--        nmaster = 1
--        delta   = 2/100
--        ratio   = 1/2
myLayout = onWorkspace "1" (avoidStruts (noBorders Full)) $
           onWorkspace "0" (noBorders (fullscreenFull Full)) $
           onWorkspace "q" (avoidStruts (noBorders tiled)) $
           onWorkspace "e" (avoidStruts (noBorders (layoutHints (tabbed shrinkText myTabConfig)))) $
           avoidStruts $ smartBorders tiled |||
           noBorders Full |||
           noBorders (layoutHints (tabbed shrinkText myTabConfig)) |||
           Grid (16/9)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 70/100
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- Eval prompt.
-- evaluate any haskell expression by pressing mod-shift-x
data EvalPrompt = EvalPrompt

instance XPrompt EvalPrompt where
  showXPrompt = const "haskell> "
  commandToComplete _ = id
  completionFunction _ s = io $ do
    res <- I.runInterpreter $ do 
        I.setImports ["Prelude", "Data.Ratio", "XMonad", "XMonad.Core"]
        I.eval s
    case res of
        Left err -> return [show err]
        Right s -> return [s]

evalPrompt :: X ()
evalPrompt = do
    uninstallSignalHandlers
    mkXPromptWithModes [XPT EvalPrompt] myXPConfig
    installSignalHandlers

-- Key bindings. Add, modify or remove key bindings here.
-- the list of kesyms may be found at /usr/include/X11/keysymdef.h
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    , ((modm,               xK_Right ), nextWS)
    , ((modm,               xK_Left  ), prevWS)

    , ((modm,               xK_x     ), runOrRaisePrompt myXPConfig)
    , ((modm .|. controlMask, xK_x   ), xmonadPrompt myXPConfig)
    , ((modm .|. shiftMask, xK_x     ), evalPrompt)

    -- close focused window
    , ((modm,               xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_Down  ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_grave ), windows W.focusUp  )
    , ((modm,               xK_Up    ), windows W.focusUp  )

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push floating window back into tiling
    , ((modm,               xK_g     ), withFocused $ windows . W.sink)

    -- Toggle window border
    , ((modm,               xK_v     ), withFocused $ toggleBorder)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)


    -- Quit xmonad
    --, ((modm .|. shiftMask, xK_z     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_z     ), spawn "xmonad --recompile; xmonad --restart")

    -- Various useful bindings
    , ((modm              , xK_F1), spawn $ helper ++ " switch pavucontrol-qt")
    , ((modm              , xK_F8), spawn "wall Hello!")
    , ((modm              , xK_F9 ), spawn "meta-f9.sh")
    , ((modm              , xK_F12 ), spawn $ helper ++ " switch picom")
    --, ((modm              , xK_F11 ), spawn "xscreensaver-command -lock")
    --, ((modm .|. shiftMask, xK_F10), spawn "scrot '%Y-%m-%d-%H-%M-%S_$wx$h.png'")
    --, ((modm              , xK_Escape ), spawn "xset dpms force off; xset dpms force off")
    --, ((0                 , xF86XK_Mail) , spawn "urxvt -e mutt")
    --, ((0                 , xF86XK_AudioNext), spawn "ncmpcpp next")       --next song
    --, ((modm              , xK_KP_Right), spawn "ncmpcpp next")       --next song
    --, ((0                 , xF86XK_AudioPrev), spawn "ncmpcpp prev")       --prev song
    --, ((modm              , xK_KP_Left), spawn "ncmpcpp prev")       --prev song
    --, ((0                 , xF86XK_AudioPlay), spawn "ncmpcpp toggle")     --toggle play/pause
    --, ((modm              , xK_KP_Home), spawn "ncmpcpp toggle")     --toggle play/pause
    --, ((0                 , xF86XK_AudioStop), spawn "ncmpcpp stop")       --stop song
    --, ((modm              , xK_KP_End), spawn "ncmpcpp stop")       --stop song
    --, ((0                 , xF86XK_AudioRaiseVolume), spawn "amixer set Master 4%+")
    --, ((0                 , xF86XK_AudioLowerVolume), spawn "amixer set Master 4%-")
    --, ((0                 , xF86XK_AudioMute), spawn "amixer set Master toggle")
    --, ((0                 , xF86XK_HomePage), spawn "firefox -new-tab https://google.com/")
    --, ((0                 , xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
    --, ((0                 , xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    --, ((0                 , xF86XK_Sleep), spawn "xscreensaver-command -lock")
    -- KDE
    --, ((modm .|. shiftMask, xK_z     ), spawn "qdbus org.kde.ksmserver /KSMServer logout 0 0 0")
    --, ((modm              , xK_f     ), spawn "qdbus org.kde.screensaver /ScreenSaver SetActive true")
    --, ((modm              , xK_backslash), spawn "dolphin")
    --, ((modm,               xK_v     ), spawn "krunner")
    --, ((modm              , xK_F2), spawn "plasmawindowed org.kde.plasma.kickoff")
    -- LXDE
    --, ((modm .|. shiftMask, xK_z     ), spawn "lxqt-leave")
    --, ((modm,               xK_v     ), spawn "lxqt-runner")
    --, ((modm              , xK_F10), spawn "/usr/bin/lximage-qt --screenshot")
    ]

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

-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "firefox"           --> doShift "1"
    , className =? "Opera"             --> doShift "1"
    , className =? "Google-chrome"     --> doShift "1"
    , className =? "Steam"             --> doShift "9"
    , className =? "dota_linux"        --> doShift "0"
    , className =? "xfreerdp"          --> doShift "0"
    , className =? "psi"               --> doShift "q"
    , className =? "Psi-plus"          --> doShift "q"
    , className =? "Psi+"              --> doShift "q"
    , className =? "Telegram"          --> doShift "q"
    , className =? "Pidgin"            --> doShift "q"
    , className =? "Thunderbird"       --> doShift "w"
    , className =? "MPlayer"           --> doFloat
    , className =? "Conky"             --> doIgnore
    , className =? "Qmmp"              --> hasBorder False -- qmmp changes it's class name 
    , className =? "qmmp"              --> hasBorder False -- from "qmmp" to "Qmmp" after start
    , (className =? "qmmp" <&&> title =? "Media Library") --> (doSink <+> hasBorder True)
    --, (role =? "gimp-toolbox" <||> role =? "gimp-dock") --> doFloat

-- IDE's
    , className =? "jetbrains-pycharm" --> doShift "e"
    , className =? "Momentics"         --> (doFloat <+> doShift "e")
    , className =? "Java"              --> (doFloat <+> doShift "e")
    , title     =? "Momentics IDE "    --> (doFloat <+> doShift "e")

-- KDE
    , isKDETrayWindow                  --> doIgnore
    , className =? "plasmashell"       --> doIgnore
    , className =? "plasmawindowed"    --> doFloat
    , className =? "krunner"           --> (doFloat <+> hasBorder False)

-- LXDE
    , className =? "Lxsession-logout"  --> doIgnore
    , className =? "Lxpanel"           --> doFloat

-- XFCE
    , className =? "Xfce4-notifyd"     --> doIgnore
    , className =? "Orage"             --> doFloat
    , className =? "Xfce4-appfinder"   --> doFloat
    ]

-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook :: Event -> X All
myEventHook = serverModeEventHook

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
              spawn $ helper ++ " respawn conky"
              spawn $ helper ++ " respawn picom"

myXmobarPP :: Handle -> PP
myXmobarPP h = xmobarPP
           { ppOutput = hPutStrLn h
           , ppCurrent = xmobarColor myActiveColor myBgColor . wrap "[" "]"
           , ppTitle = xmobarColor myHlColor myBgColor . shorten 160
           , ppSep = xmobarColor myInactiveColor myBgColor " | "
           , ppLayout = shorten 9
           }

-- Run xmonad with the settings you specify.
main :: IO ()
main = do
    -- delay .5 sec for plasma to start
    threadDelay 500000
    xmobar <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ ewmhFullscreen . ewmh $ docks $ kde4Config {
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

