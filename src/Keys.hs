module Keys ( myKeys ) where

import XMonad
import XMonad.Actions.CycleWS ( nextWS, prevWS )
import XMonad.Prompt ( XPConfig )
import XMonad.Prompt.RunOrRaise ( runOrRaisePrompt )
import XMonad.Prompt.XMonad ( xmonadPrompt )
import XMonad.Prompt.Pass ( passGeneratePrompt, passPrompt )
import XMonad.Prompt.Ssh ( sshPrompt )
import XMonad.Prompt.Zsh ( zshPrompt )
import XMonad.Actions.NoBorders ( toggleBorder )
import XMonad.Hooks.ManageDocks ( ToggleStruts(ToggleStruts) )
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.List (intercalate)

import EvalPrompt ( evalPrompt )
import Runner (switch, respawn, restart, sspawn)

myKeyMap :: XPConfig -> XConfig Layout -> [(String, X (), String)]
myKeyMap myXPConfig conf =
    [ ("M-<Return>"  , sspawn $ XMonad.terminal conf , "Launch a terminal")
    , ("M-<L>"       , nextWS                        , "Switch to next workspace")
    , ("M-<R>"       , prevWS                        , "Switch to prev workspace")
    , ("M-c"         , kill                          , "close focused window")
    , ("M-<Space>"   , sendMessage NextLayout        , "Rotate through the available layout algorithms")
    , ("M-S-<Space>" , resetLayout                   , "Reset the layouts on the current workspace to default")
    , ("M-n"         , refresh                       , "Resize viewed windows to the correct size ")
    , ("M-<Tab>"     , windows W.focusDown           , "Move focus to the next window")
    , ("M-<D>"       , windows W.focusDown           , "Move focus to the next window")
    , ("M-j"         , windows W.focusDown           , "Move focus to the next window")
    , ("M-`"         , windows W.focusUp             , "Move focus to the previous window")
    , ("M-<U>"       , windows W.focusUp             , "Move focus to the previous window")
    , ("M-k"         , windows W.focusUp             , "Move focus to the previous window")
    , ("M-m"         , windows W.focusMaster         , "Move focus to the master window")
    , ("M-S-<Return>", windows W.swapMaster          , "Swap the focused window and the master window")
    , ("M-S-j"       , windows W.swapDown            , "Swap the focused window with the next window")
    , ("M-S-k"       , windows W.swapUp              , "Swap the focused window with the previous window")
    , ("M-h"         , sendMessage Shrink            , "Shrink the master area")
    , ("M-l"         , sendMessage Expand            , "Expand the master area")
    , ("M-g"         , withFocused $ windows . W.sink, "Push floating window back into tiling")
    , ("M-f"         , withFocused float             , "Make window float")
    , ("M-v"         , withFocused toggleBorder      , "Toggle window border")
    , ("M-."         , sendMessage (IncMasterN 1)    , "Increment the number of windows in the master area")
    , ("M-,"         , sendMessage (IncMasterN (-1)) , "Deincrement the number of windows in the master area")
    , ("M-b"         , sendMessage ToggleStruts      , "Toggle the status bar gap")
    , ("M-z r"       , restartXmonad                 , "Restart xmonad ")
    , ("M-<F2>"      , switch "pavucontrol-qt"       , "Pavucontrol")
    , ("M-<F3>"      , switch "oneko"                , "Oneko")
    , ("M-<F8>"      , spawn "wall Hello!"           , "wall Hello!")
    , ("M-<F9>"      , spawn "meta-f9.sh"            , "meta-f9.sh")
    , ("M-<F12>"     , switch "picom"                , "Switch compositor")
    , ("M-x"         , runOrRaisePrompt myXPConfig   , "Run or raise prompt")
    , ("M-z z"       , zshPrompt'                    , "Zsh prompt")
    , ("M-z a"       , xmonadPrompt myXPConfig       , "Xmonad prompt")
    , ("M-z e"       , evalPrompt myXPConfig         , "Haskell evaluation prompt")
    , ("M-z s"       , sshPrompt myXPConfig          , "Ssh prompt")
    , ("M-z p"       , passPrompt myXPConfig         , "Pass prompt")
    , ("M-z ["       , passGeneratePrompt myXPConfig , "Pass prompt (generate password)")
    , ("M-bad"       , passGeneratePrompt myXPConfig , "Bad keybinding (for test only)")
    ] where
        resetLayout = setLayout $ XMonad.layoutHook conf
        restartXmonad = spawn "xmonad --recompile; xmonad --restart"
        zshPrompt' = zshPrompt myXPConfig "/home/bobr/.xmonad/capture.zsh"

myKeyMap' :: XPConfig -> XConfig Layout -> [(String, X ())]
myKeyMap' myXPConfig conf = [(i, j) | (i, j, _) <- myKeyMap myXPConfig conf]

helpMessage :: XPConfig -> XConfig Layout -> String
helpMessage myXPConfig conf = 
    intercalate "\n" [tabulate i j | (i, _, j) <- myKeyMap myXPConfig conf] ++ additional 
    where
        tabulate a b = a ++ concat (replicate x "\t") ++ b where 
            x = 3 - (length a `div` 8)
        additional = "\n\nM-z c\t\t\tCheck keybindings\n\
        \M-<F1>\t\t\tThis help message\n\n\
        \M-<workspace>\t\tSwitch to <workspace>\n\
        \M-S-<workspace>\t\tMove window to <workspace>\n\
        \M-{a,s,d}\t\tSwitch to screen 1, 2 or 3 respectively\n\
        \M-S-{a,s,d}\t\tMove window to screen 1, 2 or 3 respectively\n"

myKeys :: XPConfig -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys myXPConfig conf = mkKeymap conf $ myKeyMap' myXPConfig conf ++
    [ ("M-z c", checkKeymap conf (myKeyMap' myXPConfig conf))
    , ("M-<F1>", xmessage $ helpMessage myXPConfig conf)
    ] ++

    -- mod-N, Switch to workspace N
    -- mod-shift-N, Move client to workspace N
    [ ("M-" ++ m ++ i, windows $ f i)
    | i         <- XMonad.workspaces conf
    , (f, m)    <- [(W.greedyView, ""), (W.shift, "S-")]
    ] ++

    --mod-{a,s,d}, Switch to physical/Xinerama screens 1, 2, or 3
    --mod-shift-{a,s,d}, Move client to screen 1, 2, or 3
    [("M-" ++ m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip ["a", "s", "d"] [0..]
    , (f, m)    <- [(W.view, ""), (W.shift, "S-")]
    ]
