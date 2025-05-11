{-# LANGUAGE OverloadedStrings #-}

module UI (drawUI) where

import Brick
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Edit (renderEditor)
import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import State

drawUI :: AppState -> [Widget Name]
drawUI s = case s ^. mode of
    md
        | md `elem` [Insert, Normal] ->
            [ vBox
                [ renderEditor (str . T.unpack . T.intercalate "\n") True (s ^. bsBuffer)
                , hBorder
                , str $ modeString md ++ " | " ++ "File: " ++ (s ^. bsFilePath) ++ " | " ++ (s ^. bsMessage)
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
    Visual ->
        [ vBox
            [ renderEditor (drawEditorVisual (fromMaybe (0, 0) $ s ^. selectionRange)) True (s ^. bsBuffer)
            , hBorder
            , str $ modeString Visual ++ " | " ++ "File: " ++ (s ^. bsFilePath) ++ " | " ++ (s ^. bsMessage)
            ]
        ]
    _ -> []

drawEditorVisual :: (Int, Int) -> [T.Text] -> Widget Name
drawEditorVisual (start, end) text =
    vBox $
        [ if i >= start && i <= end
            then withAttr (attrName "selected") (str $ T.unpack line)
            else str $ T.unpack line
        | (i, line) <- zip [0 ..] text
        ]
