{-# LANGUAGE OverloadedStrings #-}

module Editor.Event (appEvent) where

import Brick
import Brick.Widgets.Edit (applyEdit, getCursorPosition, getEditContents, handleEditorEvent)
import Control.Lens
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Zipper (lineLengths, moveCursor, moveCursorClosest, textZipper)
import Editor.Features.File
import Editor.Features.Transformation
import Editor.State
import qualified Graphics.Vty as Vty
import Safe

type Keybinding = (Mode, Vty.Event, EventM Name AppState ())

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
    , (Open, Vty.EvKey Vty.KEsc [], cancelPrompt)
    , (Open, Vty.EvKey Vty.KEnter [], confirmPrompt)
    ]

keybindingsMap :: [((Mode, Vty.Event), EventM Name AppState ())]
keybindingsMap = map (\(md, ev, ac) -> ((md, ev), ac)) keybindings

exitApp :: EventM Name AppState ()
exitApp = halt

copySelection :: EventM Name AppState ()
copySelection = do
    buf <- use bsBuffer
    range <- use selectionRange
    let contents = getEditContents buf
    let (lo, hi) = fromMaybe (0, 0) range
    let selected = take (hi - lo + 1) $ drop lo contents
    bsClipboard .= selected
    enterNormal
    bsMessage .= "Copied " ++ show (length selected) ++ " lines"

deleteSelection :: EventM Name AppState ()
deleteSelection = do
    buf <- use bsBuffer
    range <- use selectionRange
    let
        (lo, hi) = fromMaybe (0, 0) range
        contents = getEditContents buf
        cursor = getCursorPosition buf
        before = take lo contents
        after = drop (hi + 1) contents
        newContents = before ++ after
     in
        do
            changeBuffer
            bsBuffer .= newEditor (T.intercalate "\n" newContents)
            use bsBuffer
                >>= (bsBuffer .=) . applyEdit (Zipper.moveCursor (max 0 $ lo - 1, snd cursor))
            enterNormal
            bsMessage .= "Deleted " ++ show (hi - lo + 1) ++ " lines"

cutSelection :: EventM Name AppState ()
cutSelection = do
    buf <- use bsBuffer
    range <- use selectionRange
    let
        (lo, hi) = fromMaybe (0, 0) range
        contents = getEditContents buf
        selected = take (hi - lo + 1) $ drop lo contents
        cursor = getCursorPosition buf
        before = take lo contents
        after = drop (hi + 1) contents
        newContents = before ++ after
     in
        do
            changeBuffer
            bsBuffer .= newEditor (T.intercalate "\n" newContents)
            use bsBuffer
                >>= (bsBuffer .=) . applyEdit (Zipper.moveCursor (max 0 $ lo - 1, snd cursor))
            enterNormal
            bsClipboard .= selected
            bsMessage .= "Cut " ++ show (hi - lo + 1) ++ " lines"

pasteClipboard :: EventM Name AppState ()
pasteClipboard = do
    buf <- use bsBuffer
    clip <- use bsClipboard
    unless (null clip) $
        let contents = getEditContents buf
            cursor = getCursorPosition buf
            (before, after) = splitAt (fst cursor + 1) contents
            newContents = before ++ clip ++ after
         in do
                changeBuffer
                bsBuffer .= newEditor (T.intercalate "\n" newContents)
                use bsBuffer
                    >>= (bsBuffer .=) . applyEdit (Zipper.moveCursor (fst cursor + length clip, snd cursor))
                bsMessage .= "Pasted " ++ show (length clip) ++ " lines"

moveCursorCol :: Int -> EventM Name AppState ()
moveCursorCol deltaX = do
    resetMessage
    buffer <- use bsBuffer
    let (y, x) = getCursorPosition buffer
    let contents = Zipper.textZipper (getEditContents buffer) Nothing
    let maxLen = Zipper.lineLengths contents !! y
    let newX = x + deltaX
     in when (newX >= 0 && newX < maxLen) $ do
            bsBuffer .= applyEdit (Zipper.moveCursor (y, x + deltaX)) buffer
            bsVirtualColumn .= newX

moveCursorRow :: Int -> EventM Name AppState ()
moveCursorRow deltaY = do
    resetMessage
    buffer <- use bsBuffer
    vCol <- use bsVirtualColumn
    let (y, _) = getCursorPosition buffer
    let contents = Zipper.textZipper (getEditContents buffer) Nothing
    let maxLen =
            let newY = y + deltaY
             in if newY >= 0
                    then max 0 $ Zipper.lineLengths contents !! newY - 1
                    else 0
    bsBuffer .= applyEdit (Zipper.moveCursor (y + deltaY, min maxLen vCol)) buffer

cancelPrompt :: EventM Name AppState ()
cancelPrompt = do
    enterNormal
    bsMessage .= "Canceled"

confirmPrompt :: EventM Name AppState ()
confirmPrompt = do
    s <- get
    case preview openPrompt s of
        Nothing -> return ()
        Just prompt -> do
            openFile $ T.unpack $ T.concat $ getEditContents prompt
            enterNormal

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

resetMessage :: EventM Name AppState ()
resetMessage = bsMessage .= ""
