{-# LANGUAGE OverloadedStrings #-}

module Editor.UI (drawUI) where

import Brick
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Edit (getEditContents, renderEditor)
import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Editor.Model
import Editor.Model.Mode
import Editor.Model.Name
import Editor.Model.State
import Safe

drawUI :: AppState -> [Widget Name]
drawUI s = case s ^. mode of
    md
        | md `elem` [Insert, Normal] ->
            [ vBox
                [ renderEditor (str . T.unpack . T.intercalate "\n") True (s ^. bsBuffer)
                , hBorder
                , str $ modeString md ++ " | " ++ "File: " ++ fileName (s ^. bsFilePath) ++ " | " ++ (s ^. bsMessage)
                ]
            ]
    Open ->
        [ vBox
            [ str "Open file: "
            , renderEditor (str . concatMap T.unpack) True (fromMaybe initialFileOpenEditor $ preview openPrompt s)
            , hBorder
            , str "Press Enter to confirm, Esc to cancel"
            ]
        ]
    Transform ->
        [ vBox
            [ str "Transform a line: "
            , renderEditor (str . concatMap T.unpack) True (fromMaybe (newTransformEditor "") $ preview transformPrompt s)
            , hBorder
            , str $ "Press Enter to confirm, Esc to cancel | Original line: " ++ show selectedFirst
            ]
        ]
    Visual ->
        [ vBox
            [ renderEditor (drawEditorVisual (fromMaybe (0, 0) $ s ^. selectionRange)) True (s ^. bsBuffer)
            , hBorder
            , str $ modeString Visual ++ " | " ++ "File: " ++ fileName (s ^. bsFilePath) ++ " | " ++ (s ^. bsMessage)
            ]
        ]
    _ -> []
  where
    selectedFirst =
        let (lo, _) = fromMaybe (0, 0) $ s ^. selectionRange
            buf = s ^. bsBuffer
            contents = getEditContents buf
         in fromMaybe "" $ headMay $ drop lo contents

drawEditorVisual :: (Int, Int) -> [T.Text] -> Widget Name
drawEditorVisual (start, end) text =
    vBox $
        [ if i >= start && i <= end
            then withAttr (attrName "selected") (str $ T.unpack line ++ "\n")
            else str $ T.unpack line ++ "\n"
        | (i, line) <- zip [0 ..] text
        ]

fileName :: FilePath -> String
fileName fp = if null fp then "[No Name]" else fp
