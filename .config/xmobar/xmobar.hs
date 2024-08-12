-- vim: fdm=marker

Config { position = TopH 19
       , font = "monospace bold 8"
       , additionalFonts = ["xft:Symbola"]
       , fgColor = "#c0c0c0"
       , commands = [ Run XMonadLog
                    , Run CommandReader "xmobar-mail" "mail"
                    , Run CommandReader "xmobar-systray" "systray"
                    , Run Cpu
                        [ "-t", "<total>"
                        , "-L", "15"
                        , "-n", "#efefef"
                        , "-H", "75", "-h", "#ff6666"
                        , "-m", "2"
                        , "-c", "0"
                        ] 30
                    , Run Memory
                        [ "-t", "<usedratio>"
                        , "-L", "15"
                        , "-n", "#efefef"
                        , "-H", "75", "-h", "#ff6666"
                        , "-m", "2"
                        , "-c", "0"
                        ] 50
                    , Run Wireless "wlan0"
                        [ "-t", "¡<quality>"
                        , "-L", "30", "-l", "#ff6666"
                        , "-n", "#feed6a"
                        , "-H", "60", "-h", "#efefef"
                        ] 50
                    , Run Kbd
                        [ ("us(cz_sk_de)", "us")
                        , ("sk(qwerty)", "<fc=#feed6a>sk</fc>")
                        , ("cz(qwerty)", "<fc=#feed6a>cz</fc>")
                        ]
                    , Run Battery
                        [ "-t", "<watts>❘<left>❘<timeleft>"
                        , "-L", "15", "-l", "#ff6666"
                        , "-n", "#efefef"
                        , "-H", "50"
                        ] 50
                    , Run Date "%a❘%d.%m❘%H:%M" "date" 600
                    ]
       , template = "%XMonadLog%}{\
                    \  %mail%\
                    \  %cpu%❘%memory%\
                    \  %wlan0wi%❘%kbd%\
                    \  %battery%\
                    \  %date%\
                    \%systray%"
       }
