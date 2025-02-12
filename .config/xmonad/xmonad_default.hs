-- vim: fdm=marker

{-# LANGUAGE FlexibleContexts #-}

-- {{{ Imports

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Graphics.X11.Xinerama (getScreenInfo)
import System.Exit
import qualified Data.Map as M

import XMonad hiding ((|||), screenCount)
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdateFocus
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.FocusTracking
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig
import XMonad.Util.Hacks
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

matchAppOrClassName :: String -> Query Bool
matchAppOrClassName n = (appName =? n) <||> (className =? n)

doCustomFloat :: ManageHook
doCustomFloat = doFloatDep move
  where
    move (W.RationalRect _ _ w h) = W.RationalRect cx cy cw ch
        where
            cx = (1 - cw) / 2
            cy = (1 - ch) / 2
            ch = h + 0.005
            cw = w + 0.005

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
        --checkKeymap myConfig myEZKeys
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
        spawn "xsession --startup"

    myLogHook = updatePointer (0.5, 0.5) (0, 0)

    myHandleEventHook = positionStoreEventHook
                        <+> hintsEventHook
                        <+> focusOnMouseMove
                        <+> trayAbovePanelEventHook
                                (className =? "trayer" <||> className =? "stalonetray")
                                (appName =? "xmobar")

    myManageHook = composeOne
                       [ appName <? "sp:" -?> customFloating $ W.RationalRect 0.02 0.03 0.96 0.94
                       , appName =? "s_aux" -?> doShift "2"
                       , appName =? "s_tmp" -?> doShift "2"
                       , appName =? "s_wrk" -?> doShift "1"
                       , appName =? "vmpk" -?> doCustomFloat
                       , appName =? "workrave" <&&> title =? "Workrave" -?> doHideIgnore
                       , className =? "Firefox (default)" <&&> appName =? "Navigator" -?> doShiftView "3"
                       , className =? "Gcr-prompter" -?> doCustomFloat
                       , className =? "Gxmessage" -?> doCustomFloat
                       , className =? "QjackCtl" -?> doCustomFloat
                       , className =? "Ssh-askpass" -?> doCustomFloat
                       , className =? "lxqt-openssh-askpass" -?> doCustomFloat
                       , className =? "mpv" -?> doShiftView "9"
                       ]
                   <+> composeAll
                       [ checkDock --> doLower
                       , isDialog --> doCenterFloat
                       ]
                   <+> positionStoreManageHook Nothing

    myLayoutHook = avoidStruts
                   $ focusTracking
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
        , ("M-u", onPrevNeighbour def W.view)
        , ("M-i", screenWorkspace 0 >>= flip whenJust (windows . W.view))
        , ("M-o", onNextNeighbour def W.view)

        , ("M-S-u", onPrevNeighbour def W.shift)
        , ("M-S-i", screenWorkspace 0 >>= flip whenJust (windows . W.shift))
        , ("M-S-o", onNextNeighbour def W.shift)

        , ("M-z", onPrevNeighbour def W.greedyView)
        , ("M-x", onNextNeighbour def W.greedyView)
        , ("M-s", moveTo Prev $ hiddenWS :&: Not emptyWS)
        , ("M-d", moveTo Next $ hiddenWS :&: Not emptyWS)
        , ("M-a", toggleWS)

          -- Stack navigation
        , ("M-j", windows W.focusDown)
        , ("M-k", windows W.focusUp)
        , ("M-<Tab>", windows W.focusDown)
        , ("M-S-<Tab>", windows W.focusUp)
        , ("M-m", windows W.focusMaster)
        , ("M-S-j", windows W.swapDown)
        , ("M-S-k", windows W.swapUp)
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
        , ("M-b", sendMessage ToggleStruts)
        , ("M-<Backspace>", focusUrgent)
        , ("M-<Space>", sendMessage NextLayout)
        , ("M-S-<Space>", setLayout $ Layout myLayoutHook)
        , ("M-h", sendMessage Shrink)
        , ("M-l", sendMessage Expand)
        , ("M-S-h", sendMessage MirrorExpand)
        , ("M-S-l", sendMessage MirrorShrink)
        , ("M-,", sendMessage $ IncMasterN 1)
        , ("M-.", sendMessage $ IncMasterN (-1))
        , ("M-t", withFocused $ windows . W.sink)
        , ("M-f M-S-f", withFocused float)
        , ("M-f M-b", sendMessage $ Toggle NOBORDERS)
        , ("M-f M-f", sendMessage $ Toggle FULL)
        , ("M-f M-r", sendMessage $ Toggle MIRROR)
        , ("M-[", withFocused $ sendMessage . maximizeRestore)
        ]

    -- }}}

----------------------------------------

myPolybar = statusBarGeneric "polybar -r top" mempty

myTaffybar  = statusBarGeneric "taffybar" mempty

myXmobar = statusBarProp "xmobar" (pure myPP)
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

main = do
    let conf = withUrgencyHook NoUrgencyHook . ewmhFullscreen . ewmh . docks $ myConfig
    --xmonad $ conf
    --xmonad . withSB myTaffybar $ conf
    --xmonad . withSB myXmobar $ conf
    xmonad . withSB myPolybar $ conf
