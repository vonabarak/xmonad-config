import Xmobar

config :: Config
config = defaultConfig
    { bgColor = "black"
    , fgColor = "green"
    , border = NoBorder
    , borderColor = "red"
    , position = Static { xpos = 550  , ypos = 0, width = 1170, height = 32 }
    -- , position = TopP 500 0
    , lowerOnStart = False
    , hideOnStart = False
    , persistent = True
    , allDesktops = True
    , overrideRedirect = True
    , font = "xft:Terminus:size=14:hinting=0:antialias=0"
    -- , font = "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-1"
    , additionalFonts = [ "xft:FontAwesome:pixelsize=12"
                        , "xft:Clockopia:pixelsize=28:hinting=0:antialias=1"
                        ]
    , commands = [ Run UnsafeStdinReader ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "<fc=#9a9a9a>| </fc>%UnsafeStdinReader%}{\
    \<fc=#ff00ff><action=`xmonadctl kill`>X</action></fc> "
    }

main :: IO ()
main = xmobar config
