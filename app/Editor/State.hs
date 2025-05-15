{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Editor.State (
    Name,
    Mode (..),
    BufferState,
    AppState,
    changeBuffer,
    bsBuffer,
    bsFilePath,
    bsMessage,
    bsClipboard,
    _NormalMode,
    _InsertMode,
    _VisualMode,
    _TransformMode,
    _OpenMode,
    initialState,
    openPrompt,
    transformPrompt,
    initialFileOpenEditor,
    newTransformEditor,
    initialEditor,
    newEditor,
    mode,
    bsVirtualColumn,
    bsUndoStack,
    bsRedoStack,
    bsLastTransformation,
    enterInsert,
    enterNormal,
    enterOpen,
    enterTransform,
    selectionRange,
    modeString,
    enterVisual,
)
where

import Brick.Types (EventM, get, put)
import Brick.Widgets.Edit (Editor, editor, getCursorPosition, getEditContents)
import Control.Lens
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Editor.Features.Transformation.Rule as TRule
import Safe

data Name = EditorName | OpenPromptName | TransformPromptName deriving (Ord, Show, Eq)

data Mode = Normal | Insert | Visual | Open | Transform deriving (Eq, Show)

modeString :: Mode -> String
modeString = map toUpper . show

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
