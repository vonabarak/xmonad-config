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

-- Always float windows
    , className =? "MPlayer"           --> doFloat
    , className =? "zoom"              --> doFloat
    , className =? "Conky"             --> doIgnore
    , className =? "Qmmp"              --> hasBorder False -- qmmp changes it's class name 
    , className =? "qmmp"              --> hasBorder False -- from "qmmp" to "Qmmp" after start
    , (className =? "qmmp" <&&> title =? "Media Library") --> (doSink <+> hasBorder True)
    , (className =? "qmmp" <&&> title =? "Qmmp Settings") --> (doSink <+> hasBorder True)
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

-- LXDE
    , className =? "Lxsession-logout"  --> doIgnore
    , className =? "Lxpanel"           --> doFloat

-- XFCE
    , className =? "Xfce4-notifyd"     --> doIgnore
    , className =? "Orage"             --> doFloat
    , className =? "Xfce4-appfinder"   --> doFloat
    ]
