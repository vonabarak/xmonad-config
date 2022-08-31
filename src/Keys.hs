module Keys ( myHotKeys ) where

import XMonad
import XMonad.Actions.CycleWS ( nextWS, prevWS )
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise ( runOrRaisePrompt )
import XMonad.Prompt.XMonad ( xmonadPrompt )
import XMonad.Prompt.Ssh ( sshPrompt )
import XMonad.Prompt.Zsh ( zshPrompt )
import XMonad.Actions.NoBorders ( toggleBorder )
import XMonad.Hooks.ManageDocks ( ToggleStruts(ToggleStruts) )
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


import Runner (switch, respawn, restart)
import EvalPrompt ( evalPrompt ) 



-- Key bindings. Add, modify or remove key bindings here.xmessage
-- the list of kesyms may be found at /usr/include/X11/keysymdef.h
myHotKeys :: XConfig Layout -> XPConfig -> [((KeyMask, KeySym), X ())]
myHotKeys conf@(XConfig {XMonad.modMask = modm}) myXPConfig = 
    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)   
    , ((modm,               xK_Right ), nextWS)
    , ((modm,               xK_Left  ), prevWS) 
    , ((modm,               xK_x     ), zshPrompt myXPConfig "/home/bobr/.xmonad/capture.zsh")
    , ((modm .|. controlMask, xK_x   ), xmonadPrompt myXPConfig)
    , ((modm .|. shiftMask, xK_x     ), evalPrompt myXPConfig)
    , ((modm .|. shiftMask, xK_f     ), sshPrompt myXPConfig)
    , ((modm .|. shiftMask, xK_g     ), runOrRaisePrompt myXPConfig)    
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
    , ((modm,               xK_grave ), windows W.focusUp)
    , ((modm,               xK_Up    ), windows W.focusUp)    
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)    
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp)    
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster)    
    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)   
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)   
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)   
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink) 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand) 
    -- Push floating window back into tiling
    , ((modm,               xK_g     ), withFocused $ windows . W.sink) 
    , ((modm              , xK_f     ), withFocused float)  
    -- Toggle window border
    , ((modm,               xK_v     ), withFocused $ toggleBorder) 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1)) 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))  
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -- Quit xmonad
    --, ((modm .|. shiftMask, xK_z     ), io (exitWith ExitSuccess))    
    -- Restart xmonad
    , ((modm              , xK_z     ), spawn "xmonad --recompile; xmonad --restart")   
    -- Various useful bindings
    , ((modm              , xK_F1    ), switch "pavucontrol-qt")
    , ((modm              , xK_F8    ), spawn "wall Hello!")
    , ((modm              , xK_F9    ), spawn "meta-f9.sh")
    , ((modm              , xK_F12   ), switch "picom")
    --, ((modm              , xK_F11   ), spawn "xscreensaver-command -lock")
    --, ((modm .|. shiftMask, xK_F10   ), spawn "scrot '%Y-%m-%d-%H-%M-%S_$wx$h.png'")
    --, ((modm              , xK_Escape), spawn "xset dpms force off; xset dpms force off")
    --, ((0                 , xF86XK_Mail     ), spawn "urxvt -e mutt")
    --, ((modm              , xK_KP_Home      ), spawn "ncmpcpp toggle")     --toggle play/pause
    --, ((modm              , xK_KP_End       ), spawn "ncmpcpp stop")       --stop song
    --, ((modm              , xK_KP_Right     ), spawn "ncmpcpp next")       --next song
    --, ((modm              , xK_KP_Left      ), spawn "ncmpcpp prev")       --prev song
    --, ((0                 , xF86XK_AudioPlay), spawn "ncmpcpp toggle")     --toggle play/pause
    --, ((0                 , xF86XK_AudioStop), spawn "ncmpcpp stop")       --stop song
    --, ((0                 , xF86XK_AudioNext), spawn "ncmpcpp next")       --next song
    --, ((0                 , xF86XK_AudioPrev), spawn "ncmpcpp prev")       --prev song
    --, ((0                 , xF86XK_AudioRaiseVolume), spawn "amixer set Master 4%+")
    --, ((0                 , xF86XK_AudioLowerVolume), spawn "amixer set Master 4%-")
    --, ((0                 , xF86XK_AudioMute), spawn "amixer set Master toggle")
    --, ((0                 , xF86XK_HomePage ), spawn "firefox -new-tab https://google.com/")
    --, ((0                 , xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
    --, ((0                 , xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    --, ((0                 , xF86XK_Sleep    ), spawn "xscreensaver-command -lock")
    -- KDE
    --, ((modm .|. shiftMask, xK_z     ), spawn "qdbus org.kde.ksmserver /KSMServer logout 0 0 0")
    --, ((modm              , xK_f     ), spawn "qdbus org.kde.screensaver /ScreenSaver SetActive true")
    --, ((modm              , xK_backslash), spawn "dolphin")
    --, ((modm,               xK_v     ), spawn "krunner")
    --, ((modm              , xK_F2), spawn "plasmawindowed org.kde.plasma.kickoff")
    -- LXDE
    --, ((modm .|. shiftMask, xK_z     ), spawn "lxqt-leave")
    --, ((modm,               xK_v     ), spawn "lxqt-runner")
    --, ((modm              , xK_F10   ), spawn "/usr/bin/lximage-qt --screenshot")
    ]