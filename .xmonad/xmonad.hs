import System.IO

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.AppendFile

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare

myStatusBar conf = statusBar "xmobar" myXmobarPP myToggleStrutsKey conf
    where
        myToggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)
        myXmobarPP = defaultPP
            {
                ppCurrent = xmobarColor "#55FF55" "" . wrap "[" "]",
                ppVisible = xmobarColor "#FEED6A" "" . wrap "(" ")",
                ppUrgent = xmobarColor "#FF5555" "#FEEF6A" . pad,
                ppSep = " ",
                ppTitle = xmobarColor "#EFEFEF" "" . shorten 100,
                ppOrder = \ (workspaces : layout : title : extras) -> [workspaces, title],
                ppSort = getSortByXineramaPhysicalRule
            }

myConfig = defaultConfig
        {
            borderWidth = 2,
            normalBorderColor = "#000000",
            focusedBorderColor = "#FF3333",
            focusFollowsMouse = True,
            clickJustFocuses = True,
            modMask = myModMask,
            terminal = myTerminal,
            workspaces = myWorkspaces,
            startupHook = setWMName "LG3D" <+> checkKeymap myConfig myKeymap,
            manageHook = myManageHook,
            layoutHook = myLayoutHook,
            logHook = updatePointer (Relative 0.5 0.5)
        }
        `removeKeysP` ["M-w", "M-S-w", "M-e", "M-S-e", "M-r", "M-S-r"]
        `additionalKeysP` myKeymap
    where
        myModMask = mod4Mask
        myTerminal = "urxvtc"
        myWorkspaces = map show [1..9]
        myScratchpads = [
                NS "term" (myTerminal ++ " -name sp_term") (resource =? "sp_term") (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
                NS "htop" (myTerminal ++ " -name sp_htop -e htop") (resource =? "sp_htop") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
            ]
        myManageHook = namedScratchpadManageHook myScratchpads <+> composeOne
            [
                isDialog -?> doCenterFloat,
                className =? "Skype" -?> doFloat
            ]
        myLayoutHook = layoutHintsWithPlacement (0.5, 0.5) myTabbed |||
                       noBorders Full |||
                       layoutHintsWithPlacement (0.5, 0.5) myTiled
            where
                aTheme = (theme deiflTheme)
                myTheme = aTheme
                    {
                        activeBorderColor = activeColor aTheme,
                        inactiveBorderColor = inactiveColor aTheme,
                        urgentBorderColor = urgentColor aTheme,
                        fontName = "xft:local_xmonad",
                        decoWidth = 500,
                        decoHeight = 15
                    }
                myTabbed = tabbedBottom shrinkText myTheme
                myTiled = Tall 1 (3/100) (1/2)
        myKeymap = [
                ("M-w", onPrevNeighbour W.view),
                ("M-e", onNextNeighbour W.view),
                ("M-S-w", onPrevNeighbour W.shift),
                ("M-S-e", onNextNeighbour W.shift),

                ("M-s", moveTo Prev HiddenNonEmptyWS),
                ("M-d", moveTo Next HiddenNonEmptyWS),

                ("M-z", onPrevNeighbour W.greedyView),
                ("M-x", onNextNeighbour W.greedyView),

                ("M-u", viewScreen 0),
                ("M-i", viewScreen 1),
                ("M-o", viewScreen 2),
                ("M-S-u", sendToScreen 0),
                ("M-S-i", sendToScreen 1),
                ("M-S-o", sendToScreen 2),

                ("M-a", toggleWS' ["NSP"]),
                ("M-S-p", warpToWindow 0.01 0.01),

                ("M-<Backspace>", focusUrgent),

                -- scratchpads
                ("M-S-t", namedScratchpadAction myScratchpads "term"),
                ("M-S-h", namedScratchpadAction myScratchpads "htop"),

                -- dmenu
                ("M-p", spawn "dmenu_run"),

                -- screenshots
                ("M-<Print>", spawn "scrot -m ~/tmp/shot-%Y-%m-%d.%s.png -e 'feh $f'"),
                ("M-S-<Print>", spawn "sleep 0.5 && scrot -m -s ~/tmp/shot-%Y-%m-%d.%s.png -e 'feh $f'"),

                -- audio
                ("M-S-[", spawn "amixer -q set Master 4- && notify-audio"),
                ("M-S-]", spawn "amixer -q set Master 4+ && notify-audio"),
                ("M-S-m", spawn "amixer -q set Master toggle && notify-audio"),
                ("M-S-n", spawn "amixer -q set Headphone,1 toggle && notify-audio-dock"),

                -- setxkbmap
                ("M-<F1>", spawn "setxkbmap us && notify-xkbmap"),
                ("M-<F2>", spawn "setxkbmap sk qwerty && notify-xkbmap"),
                ("M-<F3>", spawn "setxkbmap cz qwerty && notify-xkbmap"),

                -- selections
                ("M-<F5>", spawn "xsel | xsel -ib"),
                ("M-<F6>", spawn "xsel -b | xsel -i"),
                ("M-<F7>", spawn "xsel | xsel -i"),
                ("M-<F8>", spawn "xsel -b | xsel -ib"),

                -- other
                ("M-<F10>", spawn "xscreen && xmonad-session-repair"),
                ("M-<F12>", spawn "grabc 2>&1 | xsel -i"),
                ("M-S-<F12>", spawn "xmeasure | xsel -i"),
                ("M-S-l", spawn "sudo lockx"),
                ("M-S-b", spawn "backlight-toggle"),
                ("M-S-d", spawn "dpms-toggle && notify-dpms"),
                ("M-S-r", spawn "rfkill-toggle && notify-rfkill")
            ]

main = xmonad =<< myStatusBar (withUrgencyHook NoUrgencyHook myConfig)
