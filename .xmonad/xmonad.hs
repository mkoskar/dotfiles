-- vim: fdm=marker

{-# LANGUAGE FlexibleContexts #-}

-- {{{ Imports

import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, All(..), appEndo)
import Graphics.X11.Xinerama (getScreenInfo)
import System.Exit
import qualified Data.Map as M

import XMonad hiding ((|||), screenCount)
import XMonad.StackSet (screenDetail, current)
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.OnScreen
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ToggleHook
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowNavigation

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

-- }}}

(<?) :: Eq a => Query [a] -> [a] -> Query Bool
q <? x = fmap (isPrefixOf x) q

doShiftView :: WorkspaceId -> ManageHook
doShiftView w = doShift w <+> (doF $ W.view w)

workspaceOnScreen :: PhysicalScreen -> WorkspaceId -> X ()
workspaceOnScreen p w = do
    s <- getScreen p
    windows $ onScreen (W.greedyView w) FocusCurrent (fromMaybe 0 s)

screenCount :: X Int
screenCount = withDisplay $ io . (fmap length) . getScreenInfo

xmobarActionWrap :: String -> String -> String
xmobarActionWrap "" m = m
xmobarActionWrap a m  = wrap ("<action=`" ++ a ++ "`>") "</action>" m

matchAppOrClassName :: String -> Query Bool
matchAppOrClassName n = (appName =? n) <||> (className =? n)

myStatusBar conf = statusBar "statusbar" myPP myToggleStrutsKey conf
  where
    color = xmobarColor
    myPP = def
        { ppCurrent = color "#55ff55" "" . wrap "[" "]"
        , ppVisible = color "#feed6a" "" . wrap "(" ")"
        , ppUrgent = color "#bb4455" "#f6e972" . pad
        , ppSep = " "
        , ppTitle = color "#efefef" "" . shorten 100 . xmobarStrip
        , ppOrder = \(workspaces : layout : title : extras) -> [workspaces, title]
        , ppSort = getSortByXineramaPhysicalRule
        }
    myToggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

myConfig = def
    { borderWidth = 1
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#ff3333"
    , modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , keys = myKeys
    , startupHook = myStartupHook
    , logHook = myLogHook
    , handleEventHook = myHandleEventHook
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    }
  where
    myModMask = mod4Mask
    myTerminal = "term"
    myWorkspaces = map show [0..9]
    myScratchpads =
        [ NS "sp0" ("term -n '[sp0]' trun mux adm") (appName =? "[sp0]")
          (customFloating $ W.RationalRect 0.03 0.03 0.94 0.94)
        , NS "sp1" ("term -n '[sp1]' trun mux mon") (appName =? "[sp1]")
          (customFloating $ W.RationalRect 0.03 0.03 0.94 0.94)
        ]

    -- {{{ Hooks

    myStartupHook = do
        checkKeymap myConfig myEZKeys
        ewmhDesktopsStartup
        setWMName "LG3D"
        spawn "wmname LG3D || true"
        n <- screenCount
        case n of
            1 -> do
                workspaceOnScreen 0 "3"
            2 -> do
                workspaceOnScreen 0 "3"
                workspaceOnScreen 1 "2"
                viewScreen 0
            _ -> do
                workspaceOnScreen 0 "8"
                workspaceOnScreen 1 "3"
                workspaceOnScreen 2 "2"
                viewScreen 1
        spawn "xsession-hook startup"

    myLogHook = ewmhDesktopsLogHook
                <+> updatePointer (0.5, 0.5) (0, 0)

    myHandleEventHook = ewmhDesktopsEventHook
                        <+> docksEventHook
                        <+> minimizeEventHook
                        <+> positionStoreEventHook
                        {-<+> hintsEventHook-}

    myManageHook = composeOne
                       [ appName =? "clementine" -?> doShiftView "8"
                       , appName =? "gpodder" -?> doShift "8"
                       , appName =? "libreoffice" -?> doShiftView "8"
                       , appName =? "pinentry-gtk-2" -?> doFloat
                       , appName =? "qjackctl" <&&> title <? "JACK Audio Connection Kit" -?> doFloat
                       , appName =? "s_aux" -?> doShift "2"
                       , appName =? "s_tmp" -?> doShift "2"
                       , appName =? "s_wrk" -?> doShift "1"
                       , appName =? "skype" -?> doShiftView "9"
                       , appName =? "skypeforlinux" -?> doShiftView "9"
                       , appName =? "smplayer" -?> doShiftView "9"
                       , appName =? "spacefm" -?> doShiftView "5"
                       , appName =? "sxiv" -?> doShiftView "9"
                       , appName =? "vlc" -?> doShiftView "9"
                       , appName =? "vmpk" -?> doFloat
                       , appName =? "workrave" <&&> title =? "Workrave" -?> doHideIgnore
                       , appName =? "zathura" -?> doShiftView "8"
                       , className =? "Firefox (default)" -?> doShiftView "3"
                       , className =? "Gxmessage" -?> doFloat
                       , className =? "MPlayer" -?> doShiftView "9"
                       , className =? "Tor Browser" -?> doShiftView "3"
                       , className =? "jetbrains-idea" -?> doShiftView "1"
                       , className =? "mpv" -?> doShiftView "9"
                       , className =? "qutebrowser" -?> doShiftView "3"
                       , title =? "qiv" -?> doShiftView "9"
                       ]
                   <+> composeAll [ isDialog --> doCenterFloat ]
                   <+> toggleHook "doFloat" doFloat
                   <+> namedScratchpadManageHook myScratchpads
                   <+> positionStoreManageHook Nothing

    myLayoutHook = avoidStruts
                   $ trackFloating
                   $ boringWindows
                   $ minimize
                   $ maximize
                   $ configurableNavigation noNavigateBorders
                   $ mkToggle (single NOBORDERS)
                   $ mkToggle (single FULL)
                   $ mkToggle (single MIRROR)
                   (
                       myTabbed |||
                       myTall
                   )
      where
        myTheme = def
            { activeBorderColor = activeColor myTheme
            , inactiveBorderColor = inactiveColor myTheme
            , urgentBorderColor = urgentColor myTheme
            , activeTextColor = "#87cefa"
            , inactiveTextColor = "#888888"
            , urgentTextColor = "#bb4455"
            , activeColor = "#000000"
            , inactiveColor = "#3a3a3a"
            , urgentColor = "#f6e972"
            , fontName = "xft:local_statusbar"
            , decoWidth = 500
            , decoHeight = 15
            }
        myTabbed = tabbedBottom shrinkText myTheme
        myTall = ResizableTall 1 0.05 0.5 []

    -- }}}

    -- {{{ Keymap

    myKeys = \conf -> M.union (M.fromList myRawKeys) (mkKeymap conf myEZKeys)

    myRawKeys =
        [
        ]

    myEZKeys =
        [
          -- M-[0..9] - switch to workspace N
          -- M-S-[0..9] - move client to workspace N
          ("M" ++ m ++ "-" ++ k, windows $ f i)
            | (i, k) <- zip myWorkspaces $ map show [0..9]
            , (f, m) <- [(W.greedyView, ""), (W.shift, "-S")]
        ]
        ++
        [ ("M-q", spawn "xmonad --recompile && xmonad --restart")
        , ("M-S-q", io (exitWith ExitSuccess))

          -- Screens & Workspaces
        , ("M-w", onPrevNeighbour W.view)
        , ("M-e", onNextNeighbour W.view)
        , ("M-S-w", onPrevNeighbour W.shift)
        , ("M-S-e", onNextNeighbour W.shift)

        , ("M-z", onPrevNeighbour W.greedyView)
        , ("M-x", onNextNeighbour W.greedyView)
        , ("M-s", moveTo Prev HiddenNonEmptyWS)
        , ("M-d", moveTo Next HiddenNonEmptyWS)

        --, ("M-u", viewScreen 0)
        , ("M-i", viewScreen 0)
        , ("M-o", viewScreen 1)
        --, ("M-S-u", sendToScreen 0)
        , ("M-S-i", sendToScreen 0)
        , ("M-S-o", sendToScreen 1)

        , ("M-a", toggleWS' ["NSP"])

          -- Stack navigation
        , ("M-j", focusDown)
        , ("M-k", focusUp)
        , ("M-<Tab>", focusDown)
        , ("M-S-<Tab>", focusUp)
        , ("M-m", focusMaster)

        , ("M-S-j", windows W.swapDown) -- ! breaks BoringWindows
        , ("M-S-k", windows W.swapUp)   -- ! breaks BoringWindows
        , ("M-<Return>", windows W.swapMaster)

          -- Spatial navigation
        , ("M-C-l", sendMessage $ Go R)
        , ("M-C-h", sendMessage $ Go L)
        , ("M-C-k", sendMessage $ Go U)
        , ("M-C-j", sendMessage $ Go D)

        , ("M-C-S-l", sendMessage $ Swap R)
        , ("M-C-S-h", sendMessage $ Swap L)
        , ("M-C-S-k", sendMessage $ Swap U)
        , ("M-C-S-j", sendMessage $ Swap D)

          -- Windows & Layouts
        , ("M-n", refresh)
        , ("M-S-c", kill)
        , ("M-<Backspace>", focusUrgent)
        , ("M-<Space>", sendMessage NextLayout)
        , ("M-S-<Space>", setLayout $ Layout myLayoutHook)
        , ("M-h", sendMessage Shrink)
        , ("M-l", sendMessage Expand)
        , ("M-S-h", sendMessage MirrorShrink)
        , ("M-S-l", sendMessage MirrorExpand)
        , ("M-,", sendMessage $ IncMasterN 1)
        , ("M-.", sendMessage $ IncMasterN (-1))

          -- Floating layer
        , ("M-t", withFocused $ windows . W.sink)
        , ("M-; M-f", toggleHookNext "doFloat")
        , ("M-M1-k", withFocused $ keysMoveWindow (0, -50))
        , ("M-M1-j", withFocused $ keysMoveWindow (0, 50))
        , ("M-M1-h", withFocused $ keysMoveWindow (-50, 0))
        , ("M-M1-l", withFocused $ keysMoveWindow (50, 0))
        , ("M-M1-S-k", withFocused $ keysResizeWindow (0, -50) (0, 0))
        , ("M-M1-S-j", withFocused $ keysResizeWindow (0, 50) (0, 0))
        , ("M-M1-S-h", withFocused $ keysResizeWindow (-50, 0) (0, 0))
        , ("M-M1-S-l", withFocused $ keysResizeWindow (50, 0) (0, 0))

          -- Layout modifiers
        , ("M-f M-b", sendMessage $ Toggle NOBORDERS)
        , ("M-f M-f", sendMessage $ Toggle FULL)
        , ("M-f M-r", sendMessage $ Toggle MIRROR)

          -- Minimize / Maximize
        , ("M-f M--", withFocused minimizeWindow)
        , ("M-f M-=", sendMessage RestoreNextMinimizedWin)
        , ("M-[", withFocused (sendMessage . maximizeRestore))

          -- Other
        , ("M-y", spawn "xdotool mousedown --clearmodifiers 1 mouseup --clearmodifiers 1")
        , ("M-C-y", spawn "xdotool mousedown --clearmodifiers 2 mouseup --clearmodifiers 2")
        , ("M-S-y", spawn "xdotool mousedown --clearmodifiers 3 mouseup --clearmodifiers 3")
        , ("M-C-M1-y", warpToWindow 0.500 0.500)
        , ("M-C-M1-u", warpToWindow 0.005 0.005)
        , ("M-C-M1-i", warpToWindow 0.500 0.005)
        , ("M-C-M1-o", warpToWindow 0.995 0.005)
        , ("M-C-M1-j", warpToWindow 0.005 0.500)
        , ("M-C-M1-k", warpToWindow 0.500 0.500)
        , ("M-C-M1-l", warpToWindow 0.995 0.500)
        , ("M-C-M1-m", warpToWindow 0.005 0.995)
        , ("M-C-M1-,", warpToWindow 0.500 0.995)
        , ("M-C-M1-.", warpToWindow 0.995 0.995)

          -- Launchers
          -- ----------------------------------------

        , ("M-p", spawn "dmenu_run")
        , ("M-'", spawn "bb")
        , ("M-S-'", spawn "bb dd")
        , ("M-S-<Return>", spawn myTerminal)

          -- Scratchpads
        , ("M-S-;", namedScratchpadAction myScratchpads "sp0")
        , ("M-; M-;", namedScratchpadAction myScratchpads "sp1")

          -- Screenshots
        , ("M-<Print>", spawn "sshot")

          -- Audio
        , ("M-S-]", spawn "audio playback_up")
        , ("M-S-[", spawn "audio playback_down")
        , ("M-S-m", spawn "audio playback_toggle")
        , ("M-S-n", spawn "audio playback_dock_toggle")
        , ("M-S-r", spawn "audio capture_toggle")
        , ("M-S-s", spawn "audio pa_sink_toggle")
        , ("M-S-p", spawn "playctl play-pause")
        , ("M-C-p", spawn "playctl -b play-pause")

          -- XKB
        , ("M-<F1>", spawn "xkb 0")
        , ("M-<F2>", spawn "xkb 1")
        , ("M-<F3>", spawn "xkb 2")

          -- Selections
        , ("M-<Insert>", spawn "clip")
        , ("M-S-<Insert>", spawn "clip -p && notify 'PRIMARY -> CLIPBOARD'")

          -- Other
        , ("M-<F9>", spawn "xscreen && xmonad --restart")
        , ("M-<F10>", spawn "xscreen - && xmonad --restart")
        , ("M-S-<F10>", spawn "xscreen mobile && xmonad --restart")

        , ("M-<F12>", spawn "measure | clip -i")

        , ("M-; M-b", spawn "bluetooth-toggle")
        , ("M-; M-d", spawn "dpms-toggle")
        , ("M-; M-l", spawn "sudo lock")
        , ("M-; M-t", spawn "trackpoint-wheel toggle")
        , ("M-; M-S-t", spawn "touchpad toggle")
        , ("M-; M-w", spawn "wifi-toggle")
        , ("M-S-b", spawn "backlight-toggle")

        , ("M-; M-a", spawn "b http://apod.nasa.gov/")
        , ("M-; M-i", spawn "notify -u low \"$(status)\"")
        , ("M-; M-m", spawn "markx-url")
        , ("M-; M-S-m", spawn "markx")
        , ("M-; M-p", spawn "playx")
        , ("M-; M-S-p", spawn "playx-menu")
        , ("M-; M-s", spawn "selfie")
        , ("M-; M-r", spawn "notify -u low \"$(urlres)\"")
        , ("M-; M-u", spawn "unicode -x select fav")
        , ("M-; M-S-u", spawn "unicode -x select")
        , ("M-S-<Home>", spawn "term trun wow")

        , ("<XF86AudioNext>", spawn "playctl next")
        , ("<XF86AudioPlay>", spawn "playctl play-pause")
        , ("<XF86AudioPrev>", spawn "playctl prev")
        , ("<XF86AudioStop>", spawn "playctl stop")
        , ("<XF86Display>", spawn "xscreen && xmonad --restart")
        , ("<XF86WebCam>", spawn "selfie")
        ]

    -- }}}

main = xmonad =<< myStatusBar (withUrgencyHook NoUrgencyHook myConfig)
