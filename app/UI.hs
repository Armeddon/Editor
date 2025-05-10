{-# LANGUAGE OverloadedStrings #-}

module UI (drawUI) where

import Brick
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Edit (renderEditor)
import Control.Lens
import qualified Data.Text as T
import State

drawUI :: AppState -> [Widget Name]
drawUI s = case s ^. mode of
  Editing ->
    [ vBox
        [ renderEditor (str . T.unpack . T.intercalate "\n") True (s ^. ed),
          hBorder,
          str $ "File: " ++ (s ^. filePath) ++ " | " ++ (s ^. message)
        ]
    ]
  PromptingOpenFile ->
    [ vBox
        [ str "Open file: ",
          renderEditor (str . concatMap T.unpack) True (s ^. filePrompt),
          hBorder,
          str "Press Enter to confirm, Esc to cancel"
        ]
    ]
