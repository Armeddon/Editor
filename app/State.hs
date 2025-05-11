{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module State (
    Name,
    Mode (..),
    BufferState,
    AppState,
    bsBuffer,
    bsFilePath,
    bsMessage,
    _NormalMode,
    _InsertMode,
    _OpenMode,
    initialState,
    openPrompt,
    initialFileOpenEditor,
    initialEditor,
    newEditor,
    mode,
    bsUndoStack,
    bsRedoStack,
    enterInsert,
    enterNormal,
    enterOpen,
    selectionRange,
    modeString,
    enterVisual,
)
where

import Brick.Types (EventM, put)
import Brick.Widgets.Edit (Editor, editor, getCursorPosition)
import Control.Lens
import qualified Data.Text as T

data Name = EditorName | FilePromptName deriving (Ord, Show, Eq)

data Mode = Normal | Insert | Visual | Open deriving (Eq)

modeString :: Mode -> String
modeString Normal = "NORMAL"
modeString Insert = "INSERT"
modeString Open = "OPEN"
modeString Visual = "VISUAL"

data BufferState = BufferState
    { _buffer :: Editor T.Text Name
    , _filePath :: FilePath
    , _message :: String
    , _undoStack :: [T.Text]
    , _redoStack :: [T.Text]
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

selectionRange :: Getter AppState (Maybe (Int, Int))
selectionRange = to getSelectionRange

getSelectionRange :: AppState -> Maybe (Int, Int)
getSelectionRange (VisualMode bufferState ss) =
    let (a, b) = (ss, fst $ getCursorPosition (_buffer bufferState))
     in Just (min a b, max a b)
getSelectionRange _ = Nothing

mode :: Getter AppState Mode
mode = to getMode

getMode :: AppState -> Mode
getMode (NormalMode _) = Normal
getMode (InsertMode _) = Insert
getMode (OpenMode _ _) = Open
getMode (VisualMode _ _) = Visual

initialState :: AppState
initialState =
    NormalMode
        ( BufferState
            { _buffer = editor EditorName Nothing ""
            , _filePath = "[NO NAME]"
            , _message = "Welcome!"
            , _undoStack = []
            , _redoStack = []
            }
        )

initialFileOpenEditor :: Editor T.Text Name
initialFileOpenEditor = editor FilePromptName (Just 1) ""

initialEditor :: Editor T.Text Name
initialEditor = editor EditorName Nothing ""

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
    put $ VisualMode bufferState $ fst $ getCursorPosition bsBuffer'

enterInsert :: EventM Name AppState ()
enterInsert = do
    bufferState <- use bs
    put $ InsertMode bufferState

enterOpen :: EventM Name AppState ()
enterOpen = do
    bufferState <- use bs
    put $ OpenMode bufferState initialFileOpenEditor
