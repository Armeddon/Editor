{-# LANGUAGE OverloadedStrings #-}

module Editor.Model.Model (
    initialEditor,
    initialFileOpenEditor,
    newEditor,
    newTransformEditor,
    enterNormal,
    enterInsert,
    enterOpen,
    enterVisual,
    enterTransform,
    changeBuffer,
    resetMessage,
) where

import Brick.Types (EventM, get, put)
import Brick.Widgets.Edit (Editor, editor, getCursorPosition, getEditContents)
import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Editor.Model.Name
import Editor.Model.State
import Safe

initialFileOpenEditor :: Editor T.Text Name
initialFileOpenEditor = editor OpenPromptName (Just 1) ""

newTransformEditor :: T.Text -> Editor T.Text Name
newTransformEditor = editor TransformPromptName (Just 1)

initialEditor :: Editor T.Text Name
initialEditor = newEditor ""

newEditor :: T.Text -> Editor T.Text Name
newEditor = editor EditorName Nothing

enterNormal :: EventM Name AppState ()
enterNormal = do
    bufferState <- use bs
    put $ NormalMode bufferState

enterVisual :: EventM Name AppState ()
enterVisual = do
    bufferState <- use bs
    bsBuffer' <- use bsBuffer
    s <- get
    put $ VisualMode bufferState $ case s ^? _TransformMode of
        Just (_, ss, _) -> ss
        Nothing -> fst $ getCursorPosition bsBuffer'

enterInsert :: EventM Name AppState ()
enterInsert = do
    bufferState <- use bs
    put $ InsertMode bufferState

enterOpen :: EventM Name AppState ()
enterOpen = do
    bufferState <- use bs
    put $ OpenMode bufferState initialFileOpenEditor

enterTransform :: EventM Name AppState ()
enterTransform = do
    range <- use selectionRange
    buf <- use bsBuffer
    s <- get
    case s ^? _VisualMode of
        Nothing -> return ()
        Just (bufferState, ss) ->
            let (lo, _) = fromMaybe (0, 0) range
                contents = getEditContents buf
                selectedFirst = fromMaybe "" $ headMay $ drop lo contents
             in put $ TransformMode bufferState ss $ newTransformEditor selectedFirst

changeBuffer :: EventM Name AppState ()
changeBuffer = do
    old <- use bsBuffer
    let oldText = T.intercalate "\n" $ getEditContents old
    undoStack' <- use bsUndoStack
    bsUndoStack .= oldText : undoStack'
    bsRedoStack .= []

resetMessage :: EventM Name AppState ()
resetMessage = bsMessage .= ""
