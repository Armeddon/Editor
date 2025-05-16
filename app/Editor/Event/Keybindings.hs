module Editor.Event.Keybindings where

import Brick (EventM)
import Editor.Event.Events
import Editor.Event.Keybinding
import Editor.Features.File.Open
import Editor.Features.File.Save
import Editor.Features.Selection
import Editor.Features.Transformation
import Editor.Features.UndoRedo
import Editor.Model
import Editor.Model.Mode
import Editor.Model.Name
import Editor.Model.State
import qualified Graphics.Vty as Vty

keybindings :: [Keybinding]
keybindings =
    [ (Normal, Vty.EvKey (Vty.KChar 's') [], saveFile)
    , (Normal, Vty.EvKey (Vty.KChar 'o') [], enterOpen)
    , (Normal, Vty.EvKey Vty.KEsc [], exitApp)
    , (Normal, Vty.EvKey (Vty.KChar 'q') [], exitApp)
    , (Normal, Vty.EvKey (Vty.KChar 'u') [], undoAction)
    , (Normal, Vty.EvKey (Vty.KChar 'r') [], redoAction)
    , (Normal, Vty.EvKey (Vty.KChar 'i') [], enterInsert)
    , (Normal, Vty.EvKey (Vty.KChar 'v') [], enterVisual)
    , (Visual, Vty.EvKey (Vty.KChar 'v') [], enterNormal)
    , (Normal, Vty.EvKey (Vty.KChar 'h') [], moveCursorCol (-1))
    , (Normal, Vty.EvKey (Vty.KChar 'j') [], moveCursorRow 1)
    , (Normal, Vty.EvKey (Vty.KChar 'k') [], moveCursorRow (-1))
    , (Normal, Vty.EvKey (Vty.KChar 'l') [], moveCursorCol 1)
    , (Visual, Vty.EvKey (Vty.KChar 'j') [], moveCursorRow 1)
    , (Visual, Vty.EvKey (Vty.KChar 'k') [], moveCursorRow (-1))
    , (Visual, Vty.EvKey (Vty.KChar 'y') [], copySelection)
    , (Visual, Vty.EvKey (Vty.KChar 'x') [], deleteSelection)
    , (Visual, Vty.EvKey (Vty.KChar 'c') [], cutSelection)
    , (Normal, Vty.EvKey (Vty.KChar 'p') [], pasteClipboard)
    , (Visual, Vty.EvKey (Vty.KChar 't') [], transformSelected)
    , (Visual, Vty.EvKey (Vty.KChar 'T') [], repeatLastTransformation)
    , (Transform, Vty.EvKey Vty.KEsc [], enterVisual)
    , (Transform, Vty.EvKey Vty.KEnter [], endTransformation)
    , (Insert, Vty.EvKey Vty.KEsc [], enterNormal)
    , (Visual, Vty.EvKey Vty.KEsc [], enterNormal)
    , (Open, Vty.EvKey Vty.KEsc [], cancelOpenFilePrompt)
    , (Open, Vty.EvKey Vty.KEnter [], confirmOpenFilePrompt)
    ]

keybindingsMap :: [((Mode, Vty.Event), EventM Name AppState ())]
keybindingsMap = map (\(md, ev, ac) -> ((md, ev), ac)) keybindings
