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
    , font = "Terminus 14"
    -- , font = "DejaVu Sans Mono italic 9"
    , additionalFonts = [ "Noto Sans Mono 12"
                        , "Clockopia 28"
                        ]
    , commands = [ Run UnsafeStdinReader ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "<fc=#9a9a9a>| </fc>%UnsafeStdinReader%}{\
    \<fc=#ff00ff><action=`xmonadctl kill`>X</action></fc> "
    }

main :: IO ()
main = xmobar config
