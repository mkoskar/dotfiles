-- vim: fdm=marker

Config { font = "xft:local_statusbar"
       , additionalFonts = ["xft:Symbola"]
       , fgColor = "#c0c0c0"
       , textOffset = 13
       , border = BottomB
       , borderColor = "#000000"
       , borderWidth = 3
       , commands = [ Run StdinReader
                    , Run CommandReader "xmobar-mail" "mail"
                    , Run CommandReader "xmobar-tray" "tray"
                    , Run Cpu
                        [ "-t", "<total>"
                        , "-L", "10"
                        , "-n", "#efefef", "-H", "30"
                        , "-h", "#ff6666"
                        , "-m", "2"
                        , "-c", "0"
                        ] 30
                    , Run Memory
                        [ "-t", "<usedratio>"
                        , "-L", "10"
                        , "-n", "#efefef", "-H", "50"
                        , "-h", "#ff6666"
                        , "-m", "2"
                        , "-c", "0"
                        ] 50
                    , Run Wireless "wlan0"
                        [ "-t", "¡<quality>"
                        , "-l", "#ff6666", "-L", "30"
                        , "-n", "#feed6a", "-H", "60"
                        , "-h", "#efefef"
                        ] 50
                    , Run Kbd
                        [ ("us(cz_sk_de)", "us")
                        , ("sk(qwerty)", "<fc=#feed6a>sk</fc>")
                        , ("cz(qwerty)", "<fc=#feed6a>cz</fc>")
                        ]
                    , Run Battery
                        [ "-t", "<watts>❘<left>❘<timeleft>"
                        , "-l", "#ff6666", "-L", "15"
                        , "-n", "#efefef", "-H", "50"
                        , "--"
                        , "-L", "0"
                        , "-m", "#55ff55", "-H", "12"
                        , "-h", "#ff6666"
                        ] 50
                    , Run Date "%a❘%d.%m❘%H:%M" "date" 600
                    ]
       , template = "%StdinReader%\
                    \}{\
                    \  %mail%\
                    \  %cpu%❘%memory%\
                    \  %wlan0wi%❘%kbd%\
                    \  %battery%\
                    \  %date%\
                    \%tray%"
       }
