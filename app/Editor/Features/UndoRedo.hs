{-# LANGUAGE OverloadedStrings #-}

module Editor.Features.UndoRedo where

import Brick (EventM)
import Brick.Widgets.Edit (applyEdit, getCursorPosition, getEditContents)
import Control.Lens (use, (.=))
import Data.Text as T
import Data.Text.Zipper as Zipper
import Editor.Model
import Editor.Model.Mode
import Editor.Model.Name
import Editor.Model.State
import Safe

undoAction :: EventM Name AppState ()
undoAction = do
    undoStack' <- use bsUndoStack
    let option = headMay undoStack'
    case option of
        Just old -> do
            new <- use bsBuffer
            let cursor = getCursorPosition new
            bsBuffer .= newEditor old
            use bsBuffer
                >>= (bsBuffer .=) . applyEdit (Zipper.moveCursorClosest cursor)
            let newText = T.intercalate "\n" $ getEditContents new
            bsUndoStack .= tailSafe undoStack'
            use bsRedoStack >>= (bsRedoStack .=) . (newText :)
            bsMessage .= "Change undone"
        Nothing -> bsMessage .= "Already at oldest change"

redoAction :: EventM Name AppState ()
redoAction = do
    redoStack' <- use bsRedoStack
    let option = headMay redoStack'
    case option of
        Just new -> do
            old <- use bsBuffer
            bsBuffer .= newEditor new
            let oldText = T.intercalate "\n" $ getEditContents old
            bsRedoStack .= tailSafe redoStack'
            use bsUndoStack >>= ((bsUndoStack .=) . (oldText :))
            bsMessage .= "Change redone"
        Nothing -> bsMessage .= "Already at newest change"
