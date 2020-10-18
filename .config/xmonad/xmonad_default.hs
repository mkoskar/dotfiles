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
import XMonad.Actions.Minimize
import XMonad.Actions.OnScreen
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdateFocus
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
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
    s <- getScreen def p
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
        { ppCurrent = color "#55ff55" ""
        , ppVisible = color "#feed6a" ""
        , ppUrgent = color "#bb4455" "#f6e972" . pad
        , ppSep = " "
        , ppTitle = color "#efefef" "" . shorten 100 . xmobarStrip
        , ppOrder = \(workspaces : layout : title : extras) -> [workspaces, title]
        , ppSort = getSortByXineramaPhysicalRule def
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

    -- {{{ Hooks

    myStartupHook = do
        checkKeymap myConfig myEZKeys
        ewmhDesktopsStartup
        adjustEventInput
        setWMName "LG3D"
        n <- screenCount
        case n of
            1 -> do
                workspaceOnScreen 0 "3"
            2 -> do
                workspaceOnScreen 0 "3"
                workspaceOnScreen 1 "2"
                viewScreen def 0
            _ -> do
                workspaceOnScreen 0 "8"
                workspaceOnScreen 1 "3"
                workspaceOnScreen 2 "2"
                viewScreen def 1
        spawn "xsession-hook startup"

    myLogHook = ewmhDesktopsLogHook
                <+> updatePointer (0.5, 0.5) (0, 0)

    myHandleEventHook = ewmhDesktopsEventHook
                        <+> docksEventHook
                        <+> minimizeEventHook
                        <+> fullscreenEventHook
                        <+> positionStoreEventHook
                        <+> hintsEventHook
                        <+> focusOnMouseMove

    myManageHook = composeOne
                       [ appName <? "sp:" -?> customFloating $ W.RationalRect 0.02 0.03 0.96 0.94
                       , appName =? "clementine" -?> doShiftView "8"
                       , appName =? "gpodder" -?> doShift "8"
                       , appName =? "libreoffice" -?> doShiftView "8"
                       , appName =? "pinentry-gtk-2" -?> doFloat
                       , appName =? "qjackctl" <&&> title <? "JACK Audio Connection Kit" -?> doFloat
                       , appName =? "s_aux" -?> doShift "2"
                       , appName =? "s_tmp" -?> doShift "2"
                       , appName =? "s_wrk" -?> doShift "1"
                       , appName =? "spacefm" -?> doShiftView "5"
                       , appName =? "sxiv" -?> doShiftView "9"
                       , appName =? "vmpk" -?> doFloat
                       , appName =? "workrave" <&&> title =? "Workrave" -?> doHideIgnore
                       , appName =? "zathura" -?> doShiftView "8"
                       , className <? "Firefox" <&&> appName =? "Toolkit" -?> doFloat
                       , className =? "Firefox (default)" <&&> appName =? "Navigator" -?> doShiftView "3"
                       , className =? "Gxmessage" -?> doFloat
                       , className =? "mpv" -?> doShiftView "9"
                       , className =? "qutebrowser" -?> doShiftView "3"
                       , title =? "qiv" -?> doShiftView "9"
                       ]
                   <+> composeAll [ isDialog --> doCenterFloat ]
                   <+> positionStoreManageHook Nothing

    myLayoutHook = avoidStruts
                   -- $ layoutHints
                   $ trackFloating
                   $ boringWindows
                   $ minimize
                   $ maximize
                   $ smartBorders
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
        [ ("M-q", spawn "xmonad --restart")
        , ("M-S-q", io (exitWith ExitSuccess))
        , ("M-S-<Return>", spawn myTerminal)

          -- Screens & Workspaces
        , ("M-e", onNextNeighbour def W.view)
        , ("M-w", onPrevNeighbour def W.view)
        , ("M-S-e", onNextNeighbour def W.shift)
        , ("M-S-w", onPrevNeighbour def W.shift)
        , ("M-x", onNextNeighbour def W.greedyView)
        , ("M-z", onPrevNeighbour def W.greedyView)
        , ("M-d", moveTo Next HiddenNonEmptyWS)
        , ("M-s", moveTo Prev HiddenNonEmptyWS)
        , ("M-a", toggleWS)

        , ("M-u", viewScreen def 0)
        , ("M-i", viewScreen def 1)
        , ("M-o", viewScreen def 2)

        , ("M-S-u", sendToScreen def 0)
        , ("M-S-i", sendToScreen def 1)
        , ("M-S-o", sendToScreen def 2)

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
        , ("M-C-j", sendMessage $ Go D)
        , ("M-C-k", sendMessage $ Go U)
        , ("M-C-l", sendMessage $ Go R)
        , ("M-C-h", sendMessage $ Go L)
        , ("M-C-S-j", sendMessage $ Swap D)
        , ("M-C-S-k", sendMessage $ Swap U)
        , ("M-C-S-l", sendMessage $ Swap R)
        , ("M-C-S-h", sendMessage $ Swap L)

          -- Windows & Layouts
        , ("M-n", refresh)
        , ("M-c", withFocused killWindow)
        , ("M-<Backspace>", focusUrgent)
        , ("M-<Space>", sendMessage NextLayout)
        , ("M-S-<Space>", setLayout $ Layout myLayoutHook)
        , ("M-l", sendMessage Expand)
        , ("M-h", sendMessage Shrink)
        , ("M-S-l", sendMessage MirrorExpand)
        , ("M-S-h", sendMessage MirrorShrink)
        , ("M-,", sendMessage $ IncMasterN 1)
        , ("M-.", sendMessage $ IncMasterN (-1))
        , ("M-t", withFocused $ windows . W.sink)
        , ("M-f M-S-f", withFocused float)
        , ("M-f M-b", sendMessage $ Toggle NOBORDERS)
        , ("M-f M-f", sendMessage $ Toggle FULL)
        , ("M-f M-r", sendMessage $ Toggle MIRROR)
        , ("M-f M--", withFocused minimizeWindow)
        , ("M-f M-=", withLastMinimized maximizeWindowAndFocus)
        , ("M-[", withFocused $ sendMessage . maximizeRestore)
        ]

    -- }}}

main = xmonad =<< myStatusBar (withUrgencyHook NoUrgencyHook myConfig)
