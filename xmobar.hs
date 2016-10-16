Config { 
       font = "xft:terminus:size=8"
       --font = "xft:DejaVu Sans-7:"
       --font = "-*-terminus-*-*-*-*-12-*-*-*-*-*-iso10646-1"
       , bgColor = "black"
       , fgColor = "green"
       , border = NoBorder
       , borderColor = "red"
       --, position = Static { xpos = 300  , ypos = 200, width = 1620, height = 16 }
       , position = Static { xpos = 300  , ypos = 0, width = 1620, height = 16 }
       --, position = Static { xpos = 0  , ypos = 200, width = 1920, height = 16 }
       --, position = Static { xpos = 600  , ypos = 0, width = 1166, height = 17 }
       , lowerOnStart = True
       , hideOnStart = False
       , persistent = True
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run StdinReader
                    --, Run Weather "URMT" ["-t","<station>: <tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
                    --, Run Network "eth0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                    --, Run Network "ppp128" ["-L","0","-H","32","--normal","green","--high","red"] 5
                    --, Run Network "wlan0" ["-L","0","-H","32","--normal","green","--high","red"] 5
                    --, Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 5
                    --, Run Memory ["-t","Mem: <usedratio>%"] 10
                    --, Run Swap [] 10
                    --, Run Com "uname" ["-s","-r"] "" 0
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    --, Run Mail [("inbox", "~/.maildir")] "mail"
                    --, Run MPD ["-t", "<artist> - <title> (<album>) <track>/<plength> <statei> ", "--", "-P", ">>", "-Z", "||", "-S", "><"] 5
                    --, Run Volume "default" "Master" [ ] 10
                    , Run Kbd [ ]
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
       --, template = " %StdinReader% <fc=#9a9a9a></fc>}<fc=#ee9a00>%date%</fc>{ <fc=#0000ff>%mpd%</fc> <fc=#9a9a9a>|</fc> %default:Master% <fc=#9a9a9a>|</fc> <fc=#ffff00>%kbd%</fc> <fc=#9a9a9a>|</fc> %battery%  "
       --, template = " %StdinReader% }{ <fc=#9a9a9a>|</fc> %default:Master% <fc=#9a9a9a>|</fc> <fc=#ffff00>%kbd%</fc> <fc=#9a9a9a>|</fc> %battery%  "
       , template = "<fc=#9a9a9a>|</fc><fc=#ee9a00>%date%</fc><fc=#9a9a9a> |</fc><fc=#ffff00>%kbd%</fc><fc=#9a9a9a>|</fc> %StdinReader%}{ "
       --, template = " %StdinReader% <fc=#9a9a9a></fc>}<fc=#ee9a00>%date%</fc>} <fc=#9a9a9a>|</fc> %default:Master% <fc=#9a9a9a>|</fc> <fc=#ffff00>%kbd%</fc> <fc=#9a9a9a>|</fc> %battery%  "
       --, template = "  <fc=#9a9a9a>%StdinReader%</fc>}{"
       }

