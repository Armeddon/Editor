module Editor.Event.Events where

import Brick
import Brick.Widgets.Edit (applyEdit, getCursorPosition, getEditContents)
import Control.Lens
import Control.Monad (when)
import qualified Data.Text.Zipper as Zipper (lineLengths, moveCursor, textZipper)
import Editor.Model
import Editor.Model.Name
import Editor.Model.State (AppState (..), bsBuffer, bsVirtualColumn)

exitApp :: EventM Name AppState ()
exitApp = halt

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
