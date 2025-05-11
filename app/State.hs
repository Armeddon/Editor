{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module State (Name (..), Mode (..), AppState, ed, filePath, message, mode, filePrompt, undoStack, redoStack, initialState) where

import Brick.Widgets.Edit (Editor, editor)
import Control.Lens
import qualified Data.Text as T

data Name = EditorName | FilePromptName deriving (Ord, Show, Eq)

data Mode = Insert | PromptingOpenFile deriving (Eq)

data AppState = AppState
  { _ed :: Editor T.Text Name,
    _filePath :: FilePath,
    _message :: String,
    _mode :: Mode,
    _filePrompt :: Editor T.Text Name,
    _undoStack :: [T.Text],
    _redoStack :: [T.Text]
  }

makeLenses ''AppState

initialState :: AppState
initialState =
  AppState
    { _ed = editor EditorName Nothing "",
      _filePath = "",
      _message = "Welcome!",
      _mode = Insert,
      _filePrompt = editor FilePromptName (Just 1) "",
      _undoStack = [],
      _redoStack = []
    }
