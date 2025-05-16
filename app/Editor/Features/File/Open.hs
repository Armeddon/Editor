module Editor.Features.File.Open where

import Brick (EventM, get)
import Brick.Widgets.Edit (applyEdit, getCursorPosition, getEditContents)
import Control.Lens (preview, use, (.=))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Zipper as Zipper
import Editor.Model
import Editor.Model.Name
import Editor.Model.State
import System.IO.Error (tryIOError)

cancelOpenFilePrompt :: EventM Name AppState ()
cancelOpenFilePrompt = do
    enterNormal
    bsMessage .= "Canceled"

confirmOpenFilePrompt :: EventM Name AppState ()
confirmOpenFilePrompt = do
    s <- get
    case preview openPrompt s of
        Nothing -> return ()
        Just prompt -> do
            openFile $ T.unpack $ T.concat $ getEditContents prompt
            enterNormal

openFile :: FilePath -> EventM Name AppState ()
openFile path = do
    result <- liftIO $ tryIOError (TIO.readFile path)
    buffer <- use bsBuffer
    let cursor = getCursorPosition buffer
    bsFilePath .= path
    case result of
        Left _ -> do
            bsMessage .= "Created new file: " ++ path
            bsBuffer .= initialEditor
        Right contents -> do
            bsMessage .= "Opened " ++ path
            bsBuffer .= newEditor contents
    use bsBuffer
        >>= (bsBuffer .=) . applyEdit (Zipper.moveCursor cursor)
    bsVirtualColumn .= snd cursor
