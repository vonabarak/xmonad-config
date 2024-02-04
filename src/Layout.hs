module Layout ( myLayout ) where

import XMonad.Core
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.GridVariants
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.LayoutModifier ( ModifiedLayout )
import XMonad.Hooks.ManageDocks


myLayout myTabConfig = onWorkspace "1" workspace1 $
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
    thrCol  = avoidStruts $ ThreeCol nmaster delta (1/3)
    
    borders :: LayoutClass l a => l a -> ModifiedLayout (ConfigurableBorder Ambiguity) l a
    borders = lessBorders Screen
    --  borders = lessBorders (Combine Union Screen OnlyFloat)

    workspace1 = full ||| tiled'
    workspace0 = noBorders $ fullscreenFull Full
    workspaceW = tabbed' ||| tiled' ||| grid   ||| full
    workspaceE = tabbed' ||| tiled' ||| grid   ||| full
    workspaceR = tabbed' ||| tiled' ||| grid   ||| full
    allOthers  = tiled'  ||| grid   ||| thrCol ||| tabbed'||| full
