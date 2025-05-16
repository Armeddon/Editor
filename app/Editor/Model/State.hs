{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Editor.Model.State (
    AppState (..),
    _NormalMode,
    _InsertMode,
    _OpenMode,
    _VisualMode,
    _TransformMode,
    bs,
    bsBuffer,
    bsFilePath,
    bsMessage,
    bsUndoStack,
    bsRedoStack,
    bsVirtualColumn,
    bsClipboard,
    bsLastTransformation,
    openPrompt,
    selectionStart,
    transformPrompt,
    mode,
    selectionRange,
    initialState,
) where

import Brick.Widgets.Edit (Editor, editor, getCursorPosition)
import Control.Lens
import qualified Data.Text as T
import qualified Editor.Features.Transformation.Rule as TRule
import Editor.Model.Mode
import Editor.Model.Name

data BufferState = BufferState
    { _buffer :: Editor T.Text Name
    , _filePath :: FilePath
    , _message :: String
    , _undoStack :: [T.Text]
    , _redoStack :: [T.Text]
    , _virtualColumn :: Int
    , _clipboard :: [T.Text]
    , _lastTransformation :: Maybe TRule.TransformationRule
    }

makeLenses ''BufferState

data AppState
    = NormalMode
        { _bs :: BufferState
        }
    | InsertMode
        { _bs :: BufferState
        }
    | OpenMode
        { _bs :: BufferState
        , _openPrompt :: Editor T.Text Name
        }
    | VisualMode
        { _bs :: BufferState
        , _selectionStart :: Int
        }
    | TransformMode
        { _bs :: BufferState
        , _selectionStart :: Int
        , _transformPrompt :: Editor T.Text Name
        }

makeLenses ''AppState
makePrisms ''AppState

bsMessage :: Lens' AppState String
bsMessage = bs . message

bsFilePath :: Lens' AppState FilePath
bsFilePath = bs . filePath

bsBuffer :: Lens' AppState (Editor T.Text Name)
bsBuffer = bs . buffer

bsUndoStack :: Lens' AppState [T.Text]
bsUndoStack = bs . undoStack

bsRedoStack :: Lens' AppState [T.Text]
bsRedoStack = bs . redoStack

bsVirtualColumn :: Lens' AppState Int
bsVirtualColumn = bs . virtualColumn

bsClipboard :: Lens' AppState [T.Text]
bsClipboard = bs . clipboard

bsLastTransformation :: Lens' AppState (Maybe TRule.TransformationRule)
bsLastTransformation = bs . lastTransformation

selectionRange :: Getter AppState (Maybe (Int, Int))
selectionRange = to getSelectionRange

getSelectionRange :: AppState -> Maybe (Int, Int)
getSelectionRange (VisualMode bufferState ss) =
    let (a, b) = (ss, fst $ getCursorPosition (_buffer bufferState))
     in Just (min a b, max a b)
getSelectionRange (TransformMode bufferState ss _) =
    let (a, b) = (ss, fst $ getCursorPosition (_buffer bufferState))
     in Just (min a b, max a b)
getSelectionRange _ = Nothing

mode :: Getter AppState Mode
mode = to getMode

getMode :: AppState -> Mode
getMode (NormalMode{}) = Normal
getMode (InsertMode{}) = Insert
getMode (OpenMode{}) = Open
getMode (VisualMode{}) = Visual
getMode (TransformMode{}) = Transform

initialState :: AppState
initialState =
    NormalMode
        ( BufferState
            { _buffer = editor EditorName Nothing ""
            , _filePath = ""
            , _message = "Welcome!"
            , _undoStack = []
            , _redoStack = []
            , _virtualColumn = 0
            , _clipboard = []
            , _lastTransformation = Nothing
            }
        )
