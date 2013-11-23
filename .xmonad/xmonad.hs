-- ~/.xmonad/xmonad.hs

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
-- import XMonad.Actions.CycleRecentWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

-- import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows
-- import XMonad.Layout.DragPane
import XMonad.Layout.DwmStyle
import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutCombinators
-- import XMonad.Layout.Magnifier
-- import XMonad.Layout.Master
-- import XMonad.Layout.Maximize
-- import XMonad.Layout.Minimize
-- import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
-- import XMonad.Layout.SimpleFloat
-- import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
-- import XMonad.Layout.WindowArranger

import XMonad.Prompt
import XMonad.Prompt.AppLauncher
import XMonad.Prompt.AppendFile
-- import XMonad.Prompt.Shell
-- import XMonad.Prompt.Ssh

import XMonad.Util.EZConfig
-- import XMonad.Util.Loggers
-- import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.Themes

import System.IO

-- status bar
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
                ppOrder = \ (workspaces : layout : title : extras) -> [workspaces, title]
            }

-- config
myConfig = defaultConfig
        {
            terminal = myTerminal,
            workspaces = myWorkspaces,
            focusFollowsMouse = True,
            borderWidth = 2,
            normalBorderColor = "#000000",
            focusedBorderColor = "#FF3333",
            modMask = myModMask,
            startupHook = setWMName "LG3D" <+> checkKeymap defaultConfig myKeymap,
            manageHook = namedScratchpadManageHook myScratchpads <+> myManageHook,
            layoutHook = myLayoutHook,
            logHook = updatePointer (Relative 0.5 0.5)
        }
        `removeKeysP` [ "M-e", "M-S-e", "M-r", "M-S-r" ]
        `additionalKeysP` (myKeymap)
    where
        myTerminal = "urxvtc"
        myModMask = mod4Mask
        -- workspaces
        myWorkspaces = workspaces defaultConfig
        -- scratchpads
        myScratchpads = [
                NS "term" (myTerminal ++ " -name sp_term") (resource =? "sp_term") (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
                NS "htop" (myTerminal ++ " -name sp_htop -e htop") (resource =? "sp_htop") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
            ]
        -- manage hook
        myManageHook = composeOne
            [
                isDialog -?> doCenterFloat,
                className =? "Skype" -?> doFloat
            ]
        -- layout hook
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
                        fontName = "xft:local_xmonad_tabbed",
                        decoWidth = 500,
                        decoHeight = 15
                    }
                myTabbed = tabbedBottom shrinkText myTheme
                myTiled = Tall 1 (3/100) (1/2)
        -- keymap
        myKeymap = [
                -- wm bindings
                ("M-a", toggleWS),
                ("M-w", nextScreen),
                ("M-S-w", shiftNextScreen),
                ("M-s", moveTo Prev NonEmptyWS),
                ("M-d", moveTo Next NonEmptyWS),
                ("M-z", swapNextScreen),
                -- xterm
                ("M-x x", spawn "xterm"),
                -- dmenu
                ("M-p", spawn "dmenu_run"),
                -- lock
                ("M-S-l", spawn "lock"),
                -- scrot
                ("<Print>", spawn "scrot -e 'feh $f'"),
                ("M-<Print>", spawn "sleep 0.5 && scrot -s -e 'feh $f'"),
                -- scratchpads
                ("M-S-t", namedScratchpadAction myScratchpads "term"),
                ("M-S-h", namedScratchpadAction myScratchpads "htop"),
                -- audio
                ("M-S-j", spawn "amixer -q set Master 4- && notify-audio"),
                ("M-S-k", spawn "amixer -q set Master 4+ && notify-audio"),
                ("M-S-m", spawn "amixer -q set Master toggle && notify-audio"),
                ("M-S-n", spawn "amixer -q set Headphone,1 toggle && notify-audio-dock"),
                -- setxkbmap
                ("M-<F1>", spawn "setxkbmap us && notify-xkbmap"),
                ("M-<F2>", spawn "setxkbmap sk -variant qwerty && notify-xkbmap"),
                ("M-<F3>", spawn "setxkbmap cz -variant qwerty && notify-xkbmap"),
                -- xrandr
                ("M-<F10>", spawn "xrandr-bigdesktop && xmonad-session-repair"),
                ("M-<F11>", spawn "xrandr-bigdesktop-HDMI_HDMI && xmonad-session-repair"),
                -- other
                ("M-S-d", spawn "dpms-toggle && notify-dpms"),
                ("M-S-b", spawn "backlight-toggle"),
                ("M-S-r", spawn "rfkill-toggle && notify-rfkill"),
                ("M-<F5>", spawn "xsel | xsel -ib"),
                ("M-<F6>", spawn "xsel -b | xsel -i"),
                ("M-<F7>", spawn "xsel | xsel -i"),
                ("M-<F8>", spawn "xsel -b | xsel -ib")
            ]

-- main
main = xmonad =<< myStatusBar (withUrgencyHook NoUrgencyHook myConfig)
