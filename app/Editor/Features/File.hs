module Editor.Features.File where

import Brick (EventM)
import Brick.Widgets.Edit (getEditContents, getCursorPosition, applyEdit)
import Data.Text.Zipper as Zipper
import Editor.State
import Control.Lens (use, (.=))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import System.IO.Error (tryIOError)

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
