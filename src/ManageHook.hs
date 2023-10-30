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

module ManageHook ( myManageHook ) where

import XMonad
import XMonad.Layout.NoBorders ( hasBorder )
import XMonad.Hooks.ManageHelpers ( doSink, isKDETrayWindow )
import Data.Monoid ( Endo )

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "firefox"           --> doShift "1"
    , className =? "firefox-esr"       --> doShift "1"
    , className =? "Opera"             --> doShift "1"
    , className =? "Google-chrome"     --> doShift "1"
    , className =? "Steam"             --> doShift "9"
    , className =? "dota_linux"        --> doShift "0"
    , className =? "xfreerdp"          --> doShift "0"
    , className =? "psi"               --> doShift "q"
    , className =? "Psi-plus"          --> doShift "q"
    , className =? "Psi+"              --> doShift "q"
    , className =? "Pidgin"            --> doShift "q"
    , className =? "Telegram"          --> doShift "q"
    , className =? "TelegramDesktop"   --> doShift "q"
    , className =? "Thunderbird"       --> doShift "w"
    , className =? "Slack"             --> doShift "w"
    , className =? "Mattermost"        --> doShift "w"
    , className =? "teams-for-linux"   --> doShift "w"

-- Zoom
    , className =? "zoom" --> composeAll
        [ title   =? "zoom_linux_float_video_window" --> doIgnore      -- mini window
        , title   =? "Zoom" --> doSink                                 -- video window
        , title   =? "Zoom Meeting" --> doSink                         -- main window
        , stringProperty "_NET_WM_NAME" =? "zoom" --> hasBorder False  -- notification popup
        , doShift "q"
        , doFloat
        ]

-- Always float windows
    , className =? "MPlayer"           --> doFloat
    , className =? "Conky"             --> doIgnore
     -- qmmp changes it's class name from "qmmp" to "Qmmp" after start
    , (className =? "qmmp" <||> className =? "Qmmp") --> 
        ((appName =? "player" <||> appName =? "playlist" <||> appName =? "equalizer") -->
            (doFloat <+> hasBorder False))
    --, (role =? "gimp-toolbox" <||> role =? "gimp-dock") --> doFloat

-- IDE's
    , className =? "jetbrains-pycharm" --> doShift "e"
    , className =? "Momentics"         --> (doFloat <+> doShift "e")
    , className =? "Java"              --> (doFloat <+> doShift "e")
    , title     =? "Momentics IDE "    --> (doFloat <+> doShift "e")

-- KDE
    , isKDETrayWindow                  --> doIgnore
    , (className =? "plasmashell" <&&> title =? "Plasma") --> doIgnore
    , className =? "plasmashell"       --> doFloat
    , className =? "plasmawindowed"    --> doFloat
    , className =? "krunner"           --> (doFloat <+> hasBorder False)
    , (className =? "spectacle" <&&> stringProperty "_NET_WM_NAME" =? "Spectacle") --> doIgnore
    , className =? "spectacle"         --> doFloat

-- LXDE
    , className =? "Lxsession-logout"  --> doIgnore
    , className =? "Lxpanel"           --> doFloat

-- XFCE
    , className =? "Xfce4-notifyd"     --> doIgnore
    , className =? "Orage"             --> doFloat
    , className =? "Xfce4-appfinder"   --> doFloat
    ]
