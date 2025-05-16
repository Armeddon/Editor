module Editor.Features.File.Save where

import Brick (EventM)
import Brick.Widgets.Edit (getEditContents)
import Control.Lens (use, (.=))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Editor.Model.Name
import Editor.Model.State
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
