{-# LANGUAGE OverloadedStrings #-}

module Editor.Features.Selection where

import Brick (EventM)
import Brick.Widgets.Edit (applyEdit, getCursorPosition, getEditContents)
import Control.Lens
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Zipper
import Editor.Model
import Editor.Model.Mode
import Editor.Model.Name
import Editor.Model.State

copySelection :: EventM Name AppState ()
copySelection = do
    buf <- use bsBuffer
    range <- use selectionRange
    let contents = getEditContents buf
    let (lo, hi) = fromMaybe (0, 0) range
    let selected = take (hi - lo + 1) $ drop lo contents
    bsClipboard .= selected
    enterNormal
    bsMessage .= "Copied " ++ show (length selected) ++ " lines"

deleteSelection :: EventM Name AppState ()
deleteSelection = do
    buf <- use bsBuffer
    range <- use selectionRange
    let
        (lo, hi) = fromMaybe (0, 0) range
        contents = getEditContents buf
        cursor = getCursorPosition buf
        before = take lo contents
        after = drop (hi + 1) contents
        newContents = before ++ after
     in
        do
            changeBuffer
            bsBuffer .= newEditor (T.intercalate "\n" newContents)
            use bsBuffer
                >>= (bsBuffer .=) . applyEdit (Zipper.moveCursor (max 0 $ lo - 1, snd cursor))
            enterNormal
            bsMessage .= "Deleted " ++ show (hi - lo + 1) ++ " lines"

cutSelection :: EventM Name AppState ()
cutSelection = do
    buf <- use bsBuffer
    range <- use selectionRange
    let
        (lo, hi) = fromMaybe (0, 0) range
        contents = getEditContents buf
        selected = take (hi - lo + 1) $ drop lo contents
        cursor = getCursorPosition buf
        before = take lo contents
        after = drop (hi + 1) contents
        newContents = before ++ after
     in
        do
            changeBuffer
            bsBuffer .= newEditor (T.intercalate "\n" newContents)
            use bsBuffer
                >>= (bsBuffer .=) . applyEdit (Zipper.moveCursor (max 0 $ lo - 1, snd cursor))
            enterNormal
            bsClipboard .= selected
            bsMessage .= "Cut " ++ show (hi - lo + 1) ++ " lines"

pasteClipboard :: EventM Name AppState ()
pasteClipboard = do
    buf <- use bsBuffer
    clip <- use bsClipboard
    unless (null clip) $
        let contents = getEditContents buf
            cursor = getCursorPosition buf
            (before, after) = splitAt (fst cursor + 1) contents
            newContents = before ++ clip ++ after
         in do
                changeBuffer
                bsBuffer .= newEditor (T.intercalate "\n" newContents)
                use bsBuffer
                    >>= (bsBuffer .=) . applyEdit (Zipper.moveCursor (fst cursor + length clip, snd cursor))
                bsMessage .= "Pasted " ++ show (length clip) ++ " lines"
