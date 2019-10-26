-- vim: fdm=marker

import XMonad

myConfig = def
    { borderWidth = 1
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#ff3333"
    , modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    }
  where
    myModMask = mod4Mask
    myTerminal = "term"
    myWorkspaces = map show [0..9]

main = xmonad myConfig
