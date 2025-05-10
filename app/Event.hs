{-# LANGUAGE OverloadedStrings #-}

module Event (appEvent) where

import Brick
import Brick.Widgets.Edit (editor, getEditContents, handleEditorEvent)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as Vty
import Safe
import State (AppState (..), Mode (..), Name (..), ed, filePath, filePrompt, message, mode, redoStack, undoStack)
import System.IO.Error (tryIOError)

type Keybinding = (Mode, Vty.Event, EventM Name AppState ())

keybindings :: [Keybinding]
keybindings =
  [ (Editing, Vty.EvKey (Vty.KChar 's') [Vty.MCtrl], saveFile),
    (Editing, Vty.EvKey (Vty.KChar 'o') [Vty.MCtrl], enterOpenPrompt),
    (Editing, Vty.EvKey Vty.KEsc [], exitApp),
    (PromptingOpenFile, Vty.EvKey Vty.KEsc [], cancelPrompt),
    (PromptingOpenFile, Vty.EvKey Vty.KEnter [], confirmPrompt),
    (Editing, Vty.EvKey (Vty.KChar 'z') [Vty.MCtrl], undoAction),
    (Editing, Vty.EvKey (Vty.KChar 'y') [Vty.MCtrl], redoAction)
  ]

keybindingsMap :: [((Mode, Vty.Event), EventM Name AppState ())]
keybindingsMap = map (\(md, ev, ac) -> ((md, ev), ac)) keybindings

saveFile :: EventM Name AppState ()
saveFile = do
  ed' <- use ed
  path <- use filePath
  let contents = T.unlines $ getEditContents ed'
  result <- liftIO $ tryIOError (TIO.writeFile path contents)
  case result of
    Left _ ->
      message .= "Failed to save " ++ path
    Right _ -> do
      message .= "Saved " ++ path

enterOpenPrompt :: EventM Name AppState ()
enterOpenPrompt = mode .= PromptingOpenFile

exitApp :: EventM Name AppState ()
exitApp = halt

cancelPrompt :: EventM Name AppState ()
cancelPrompt = do
  mode .= Editing
  message .= "Canceled"

confirmPrompt :: EventM Name AppState ()
confirmPrompt = do
  prompt <- use filePrompt
  let path = T.unpack $ T.concat $ getEditContents prompt
  result <- liftIO $ tryIOError (TIO.readFile path)
  case result of
    Left _ -> do
      mode .= Editing
      message .= "Created new file" ++ path
      ed .= editor EditorName Nothing ""
      filePath .= path
    Right contents -> do
      mode .= Editing
      message .= "Opened " ++ path
      ed .= editor EditorName Nothing contents
      filePath .= path

undoAction :: EventM Name AppState ()
undoAction = do
  undoStack' <- use undoStack
  let option = headMay undoStack'
  case option of
    Just old -> do
      new <- use ed
      ed .= editor EditorName Nothing old
      let newText = T.intercalate "\n" $ getEditContents new
      undoStack .= tailSafe undoStack'
      redoStack' <- use redoStack
      redoStack .= newText : redoStack'
    Nothing -> return ()

redoAction :: EventM Name AppState ()
redoAction = do
  redoStack' <- use redoStack
  let option = headMay redoStack'
  case option of
    Just new -> do
      old <- use ed
      ed .= editor EditorName Nothing new
      let oldText = T.intercalate "\n" $ getEditContents old
      redoStack .= tailSafe redoStack'
      undoStack' <- use undoStack
      undoStack .= oldText : undoStack'
    Nothing -> return ()

appEvent :: BrickEvent Name () -> EventM Name AppState ()
appEvent (VtyEvent ev) = do
  mode' <- use mode
  Data.Maybe.fromMaybe (fallbackEvent ev) $ lookup (mode', ev) keybindingsMap
appEvent ev = do
  mode' <- use mode
  case mode' of
    Editing -> Brick.zoom ed $ handleEditorEvent ev
    PromptingOpenFile -> Brick.zoom filePrompt $ handleEditorEvent ev

fallbackEvent :: Vty.Event -> EventM Name AppState ()
fallbackEvent ev = do
  mode' <- use mode
  case mode' of
    Editing -> do
      old <- use ed
      let oldText = T.intercalate "\n" $ getEditContents old
      undoStack' <- use undoStack
      undoStack .= oldText : undoStack'
      redoStack .= []
      message .= ""
      Brick.zoom ed $ handleEditorEvent (VtyEvent ev)
    PromptingOpenFile -> Brick.zoom filePrompt $ handleEditorEvent (VtyEvent ev)
