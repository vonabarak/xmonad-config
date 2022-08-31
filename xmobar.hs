Config 
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
    , commands = [ Run UnsafeStdinReader
                 --, Run Weather "URMT" ["-t","<station>: <tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
                 --, Run Network "eth0" ["-L","0","-H","32","--normal","green","--high","red"] 5
                 --, Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 5
                 --, Run Memory ["-t","Mem: <usedratio>%"] 10
                 --, Run Swap [] 10
                 --, Run Com "uname" ["-s","-r"] "" 0
                 --, Run Date "%H:%M:%S" "clock" 10
                 --, Run Date "%a %_d %b" "date" 600
                 --, Run Mail [("inbox", "~/.maildir")] "mail"
                 --, Run MPD ["-t", "<artist> - <title> (<album>) <track>/<plength> <statei> ", "--", "-P", ">>", "-Z", "||", "-S", "><"] 5
                 --, Run Volume "default" "Master" [ ] 10
                 --, Run Kbd [ ]
                 --, Run BatteryP ["BAT0"]
                 --    ["-t", "<acstatus> <left>% <timeleft>",
                 --    "-L", "10", "-H", "80", "-p", "3",
                 --    "--", "-O", "<fc=green>AC</fc> - ", "-o", "<fc=red>BAT</fc>",
                 --    -- "-c", "energy_full",
                 --    "-f", "ADP1/online",
                 --    "-L", "-15", "-H", "-5",
                 --    "-l", "red", "-m", "blue", "-h", "green"]
                 --    50
                 ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "<fc=#9a9a9a>| </fc>%UnsafeStdinReader%}{\
    \<fc=#ff00ff><action=`xmonadctl kill`>X</action></fc> "
    }

