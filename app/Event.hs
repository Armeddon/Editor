{-# LANGUAGE OverloadedStrings #-}

module Event (appEvent) where

import Brick
import Brick.Widgets.Edit (applyEdit, getCursorPosition, getEditContents, handleEditorEvent)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Zipper as Zipper (moveCursor)
import qualified Graphics.Vty as Vty
import Safe
import State
import System.IO.Error (tryIOError)

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
    , (Normal, Vty.EvKey (Vty.KChar 'h') [], moveCursor 0 (-1))
    , (Normal, Vty.EvKey (Vty.KChar 'j') [], moveCursor 1 0)
    , (Normal, Vty.EvKey (Vty.KChar 'k') [], moveCursor (-1) 0)
    , (Normal, Vty.EvKey (Vty.KChar 'l') [], moveCursor 0 1)
    , (Visual, Vty.EvKey (Vty.KChar 'h') [], moveCursor 0 (-1))
    , (Visual, Vty.EvKey (Vty.KChar 'j') [], moveCursor 1 0)
    , (Visual, Vty.EvKey (Vty.KChar 'k') [], moveCursor (-1) 0)
    , (Visual, Vty.EvKey (Vty.KChar 'l') [], moveCursor 0 1)
    , (Insert, Vty.EvKey Vty.KEsc [], enterNormal)
    , (Visual, Vty.EvKey Vty.KEsc [], enterNormal)
    , (Open, Vty.EvKey Vty.KEsc [], cancelPrompt)
    , (Open, Vty.EvKey Vty.KEnter [], confirmPrompt)
    ]

keybindingsMap :: [((Mode, Vty.Event), EventM Name AppState ())]
keybindingsMap = map (\(md, ev, ac) -> ((md, ev), ac)) keybindings

saveFile :: EventM Name AppState ()
saveFile = do
    path <- use bsFilePath
    buffer <- use bsBuffer
    let contents = T.unlines $ getEditContents buffer
    result <- liftIO $ tryIOError (TIO.writeFile path contents)
    case result of
        Left _ ->
            bsMessage .= "Failed to save " ++ path
        Right _ -> do
            bsMessage .= "Saved " ++ path

exitApp :: EventM Name AppState ()
exitApp = halt

moveCursor :: Int -> Int -> EventM Name AppState ()
moveCursor deltaX deltaY = do
    buffer <- use bsBuffer
    let (x, y) = getCursorPosition buffer
    bsBuffer .= applyEdit (Zipper.moveCursor (x + deltaX, y + deltaY)) buffer

cancelPrompt :: EventM Name AppState ()
cancelPrompt = do
    enterNormal
    bsMessage .= "Canceled"

confirmPrompt :: EventM Name AppState ()
confirmPrompt = do
    s <- get
    let prompt = preview openPrompt s
    case prompt of
        Nothing -> return ()
        Just prompt' -> do
            let path = T.unpack $ T.concat $ getEditContents prompt'
            result <- liftIO $ tryIOError (TIO.readFile path)
            case result of
                Left _ -> do
                    enterNormal
                    bsMessage .= "Created new file: " ++ path
                    bsBuffer .= initialEditor
                    bsFilePath .= path
                Right contents -> do
                    enterNormal
                    bsMessage .= "Opened " ++ path
                    bsBuffer .= newEditor contents
                    bsFilePath .= path

undoAction :: EventM Name AppState ()
undoAction = do
    undoStack' <- use bsUndoStack
    let option = headMay undoStack'
    case option of
        Just old -> do
            new <- use bsBuffer
            bsBuffer .= newEditor old
            let newText = T.intercalate "\n" $ getEditContents new
            bsUndoStack .= tailSafe undoStack'
            redoStack' <- use bsRedoStack
            bsRedoStack .= newText : redoStack'
        Nothing -> return ()

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
            undoStack' <- use bsUndoStack
            bsUndoStack .= oldText : undoStack'
        Nothing -> return ()

appEvent :: BrickEvent Name () -> EventM Name AppState ()
appEvent (VtyEvent ev) = do
    mode' <- use mode
    Data.Maybe.fromMaybe (fallbackEvent ev) $ lookup (mode', ev) keybindingsMap
appEvent _ = return ()

fallbackEvent :: Vty.Event -> EventM Name AppState ()
fallbackEvent ev = do
    mode' <- use mode
    case mode' of
        Insert -> do
            old <- use bsBuffer
            let oldText = T.intercalate "\n" $ getEditContents old
            undoStack' <- use bsUndoStack
            bsUndoStack .= oldText : undoStack'
            bsRedoStack .= []
            bsMessage .= ""
            Brick.zoom bsBuffer $ handleEditorEvent (VtyEvent ev)
        Open -> Brick.zoom openPrompt $ handleEditorEvent (VtyEvent ev)
        _ -> return ()
