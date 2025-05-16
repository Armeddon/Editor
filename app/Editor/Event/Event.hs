{-# LANGUAGE OverloadedStrings #-}

module Editor.Event.Event (appEvent) where

import Brick
import Brick.Widgets.Edit (handleEditorEvent)
import Control.Lens
import Data.Maybe (fromMaybe)
import Editor.Event.Keybindings
import Editor.Model
import Editor.Model.Mode
import Editor.Model.Name
import Editor.Model.State
import qualified Graphics.Vty as Vty

appEvent :: BrickEvent Name () -> EventM Name AppState ()
appEvent (VtyEvent ev) = do
    mode' <- use mode
    fromMaybe (fallbackEvent ev) $ lookup (mode', ev) keybindingsMap
appEvent _ = return ()

fallbackEvent :: Vty.Event -> EventM Name AppState ()
fallbackEvent ev = do
    mode' <- use mode
    resetMessage
    case mode' of
        Insert -> do
            changeBuffer
            bsMessage .= ""
            Brick.zoom bsBuffer $ handleEditorEvent (VtyEvent ev)
        Open -> Brick.zoom openPrompt $ handleEditorEvent (VtyEvent ev)
        Transform -> Brick.zoom transformPrompt $ handleEditorEvent (VtyEvent ev)
        _ -> return ()
