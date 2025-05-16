module Editor.Event.Keybinding (Keybinding) where

import Brick (EventM)
import Editor.Model.Mode
import Editor.Model.Name
import Editor.Model.State
import qualified Graphics.Vty as Vty

type Keybinding = (Mode, Vty.Event, EventM Name AppState ())
