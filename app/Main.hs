{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Control.Monad (void)
import Event (appEvent)
import qualified Graphics.Vty as Vty
import State (AppState, Name, initialState)
import UI (drawUI)

app :: App AppState () Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap Vty.defAttr [(attrName "selected", bg Vty.brightBlack)]
        }

main :: IO ()
main = do
    void $ defaultMain app initialState
