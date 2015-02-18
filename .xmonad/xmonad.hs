-- vim: fdm=marker

-- {{{ Imports

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

doShiftView :: WorkspaceId -> ManageHook
doShiftView w = doShift w <+> (doF $ W.view w)

workspaceOnScreen :: PhysicalScreen -> WorkspaceId -> X ()
workspaceOnScreen p w = do
    s <- getScreen p
    windows $ onScreen (W.greedyView w) FocusCurrent (fromMaybe 0 s)

screenCount :: X Int
screenCount = withDisplay $ io . (fmap length) . getScreenInfo

myStatusBar conf = statusBar "statusbar" myPP myToggleStrutsKey conf
  where
    --color = dzenColor
    color = xmobarColor
    myPP = defaultPP
        { ppCurrent = color "#55FF55" "" . wrap "[" "]"
        , ppVisible = color "#FEED6A" "" . wrap "(" ")"
        , ppUrgent = color "#BB4455" "#FEEF6A" . pad
        , ppSep = " "
        , ppTitle = color "#EFEFEF" "" . shorten 100
        , ppOrder = \(workspaces : layout : title : extras) -> [workspaces, title]
        , ppSort = getSortByXineramaPhysicalRule
        }
    myToggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

myConfig = defaultConfig
    { borderWidth = 1
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#FF3333"
    , modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , keys = \conf -> mkKeymap conf myKeymap
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
        [ NS "sp0" (myTerminal ++ " sp0 - trun s adm") (resource =? "sp0")
          (customFloating $ W.RationalRect 0.03 0.03 0.94 0.94)
        , NS "sp1" (myTerminal ++ " sp1 - trun s mon") (resource =? "sp1")
          (customFloating $ W.RationalRect 0.03 0.03 0.94 0.94)
        ]

    -- {{{ Hooks

    myStartupHook = do
        checkKeymap myConfig myKeymap
        ewmhDesktopsStartup
        setWMName "LG3D"
        spawn "xsession-hook startup"
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

    myLogHook = ewmhDesktopsLogHook
                <+> updatePointer (Relative 0.5 0.5)

    myHandleEventHook = ewmhDesktopsEventHook
                        <+> docksEventHook
                        <+> minimizeEventHook
                        <+> positionStoreEventHook
                        {-<+> hintsEventHook-}

    myManageHook = composeOne
                       [ appName =? "workrave" <&&> title =? "Workrave" -?> doHideIgnore
                       , appName =? "clementine" -?> doShiftView "8"
                       , appName =? "gpodder" -?> doShift "8"
                       , appName =? "libreoffice" -?> doShiftView "8"
                       , appName =? "s_aux" -?> doShift "2"
                       , appName =? "s_tmp" -?> doShift "2"
                       , appName =? "s_wrk" -?> doShift "1"
                       , appName =? "skype" -?> doShiftView "9"
                       , appName =? "smplayer" -?> doShiftView "9"
                       , appName =? "spacefm" -?> doShiftView "5"
                       , appName =? "sxiv" -?> doShiftView "9"
                       , appName =? "vlc" -?> doShiftView "9"
                       , appName =? "zathura" -?> doShiftView "8"
                       , className =? "Firefox (default)" -?> doShiftView "3"
                       , className =? "MPlayer" -?> doShiftView "9"
                       , className =? "Tor Browser" -?> doShiftView "3"
                       , className =? "mpv" -?> doShiftView "9"
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
                   --layoutHintsWithPlacement (0.5, 0.5)
                   (
                       myTabbed |||
                       myTall
                   )
      where
        myTheme = defaultTheme
            { activeBorderColor = activeColor myTheme
            , inactiveBorderColor = inactiveColor myTheme
            , urgentBorderColor = urgentColor myTheme
            , activeTextColor = "#87CEFA"
            , inactiveTextColor = "#888888"
            , urgentTextColor = "#BB4455"
            , activeColor = "#000000"
            , inactiveColor = "#333333"
            , urgentColor = "#FEEF6A"
            , fontName = "xft:local_xmonad"
            , decoWidth = 500
            , decoHeight = 15
            }
        myTabbed = tabbedBottom shrinkText myTheme
        myTall = ResizableTall 1 0.05 0.5 []

    -- }}}

    -- {{{ Keymap

    myKeymap =
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

          -- screens & workspaces
        , ("M-w", onPrevNeighbour W.view)
        , ("M-e", onNextNeighbour W.view)
        , ("M-S-w", onPrevNeighbour W.shift)
        , ("M-S-e", onNextNeighbour W.shift)

        , ("M-z", onPrevNeighbour W.greedyView)
        , ("M-x", onNextNeighbour W.greedyView)
        , ("M-s", moveTo Prev HiddenNonEmptyWS)
        , ("M-d", moveTo Next HiddenNonEmptyWS)

        , ("M-u", viewScreen 0)
        , ("M-i", viewScreen 1)
        , ("M-o", viewScreen 2)
        , ("M-S-u", sendToScreen 0)
        , ("M-S-i", sendToScreen 1)
        , ("M-S-o", sendToScreen 2)

        , ("M-a", toggleWS' ["NSP"])

          -- stack navigation
        , ("M-j", focusDown)
        , ("M-k", focusUp)
        , ("M-<Tab>", focusDown)
        , ("M-S-<Tab>", focusUp)
        , ("M-m", focusMaster)

        , ("M-S-j", windows W.swapDown) -- ! breaks BoringWindows
        , ("M-S-k", windows W.swapUp)   -- ! breaks BoringWindows
        , ("M-<Return>", windows W.swapMaster)

          -- spatial navigation
        , ("M-C-l", sendMessage $ Go R)
        , ("M-C-h", sendMessage $ Go L)
        , ("M-C-k", sendMessage $ Go U)
        , ("M-C-j", sendMessage $ Go D)

        , ("M-C-S-l", sendMessage $ Swap R)
        , ("M-C-S-h", sendMessage $ Swap L)
        , ("M-C-S-k", sendMessage $ Swap U)
        , ("M-C-S-j", sendMessage $ Swap D)

          -- windows & layouts
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

          -- floating layer
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

          -- layout modifiers
        , ("M-f M-b", sendMessage $ Toggle NOBORDERS)
        , ("M-f M-f", sendMessage $ Toggle FULL)
        , ("M-f M-r", sendMessage $ Toggle MIRROR)

          -- minimize, maximize
        , ("M-f M--", withFocused minimizeWindow)
        , ("M-f M-=", sendMessage RestoreNextMinimizedWin)
        , ("M-[", withFocused (sendMessage . maximizeRestore))

          -- other
        , ("M-S-y", warpToWindow 0.005 0.005)
        , ("M-C-y", warpToWindow 0.995 0.995)
        , ("M-y", spawn "xdotool getwindowfocus click --clearmodifiers 1")

          -- Launchers
          -- =========

        , ("M-p", spawn "dmenu_run")
        , ("M-'", spawn "bb")
        , ("M-S-'", spawn "bb dd")
        , ("M-S-<Return>", spawn myTerminal)

          -- scratchpads
        , ("M-S-;", namedScratchpadAction myScratchpads "sp0")
        , ("M-; M-;", namedScratchpadAction myScratchpads "sp1")

          -- screenshots
        , ("M-<Print>", spawn "sshot")
        , ("M-S-<Print>", spawn "sshot -s")

          -- audio
        , ("M-S-]", spawn "audio playback_up")
        , ("M-S-[", spawn "audio playback_down")
        , ("M-S-m", spawn "audio playback_toggle")
        , ("M-S-n", spawn "audio playback_dock_toggle")
        , ("M-S-r", spawn "audio capture_toggle")
        , ("M-S-p", spawn "playctl play-pause")
        , ("M-C-p", spawn "playctl-bluetooth play-pause")

          -- setxkbmap
        , ("M-<F1>", spawn "xkb us")
        , ("M-<F2>", spawn "xkb sk")
        , ("M-<F3>", spawn "xkb cz")

          -- selections
        , ("M-<Insert>", spawn "xclipboard")
        , ("M-S-<Insert>", spawn "xclipboard from-primary && notify 'PRIMARY -> CLIPBOARD'")

          -- other
        , ("M-<F9>", spawn "xscreen0 && xmonad-session-repair")
        , ("M-<F10>", spawn "xscreen && xmonad-session-repair")
        , ("M-S-<F10>", spawn "xscreen-mobile && xmonad-session-repair")

        , ("M-<F12>", spawn "grabc 2>&1 | xclipboard from-stdin")
        , ("M-S-<F12>", spawn "xmeasure | xclipboard from-stdin")

        , ("M-; M-b", spawn "bluetooth-toggle")
        , ("M-; M-d", spawn "dpms-toggle")
        , ("M-; M-l", spawn "sudo lockx")
        , ("M-; M-w", spawn "wifi-toggle")
        , ("M-S-b", spawn "backlight-toggle")

        , ("M-; M-a", spawn "b http://apod.nasa.gov/")
        , ("M-; M-i", spawn "notify -u low \"$(status)\"")
        , ("M-; M-p", do workspaceOnScreen 0 "9"; spawn "playx")
        , ("M-S-<Home>", spawn $ myTerminal ++ " - trun wow")
        ]

    -- }}}

main = xmonad =<< myStatusBar (withUrgencyHook NoUrgencyHook myConfig)
