-- vim: fdm=marker

import Data.Default (def)

import System.Taffybar
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget

workspaces = workspacesNew def

clock = textClockNewWith def

myConfig = def
    { startWidgets = [ workspaces ]
    , endWidgets = [ clock ]
    }

main = do
    simpleTaffybar myConfig
