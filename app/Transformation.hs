{-# LANGUAGE OverloadedStrings #-}

module Transformation (transformSelected, endTransformation) where

import Brick
import Brick.Widgets.Edit (applyEdit, getCursorPosition, getEditContents)
import Control.Lens
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Zipper
import Safe (headMay, tailSafe)
import State

tokenize :: T.Text -> [T.Text]
tokenize = T.words

data TokenExpr = From Int | Literal T.Text deriving (Eq, Show)

inferTokenExpr :: [T.Text] -> [T.Text] -> [TokenExpr]
inferTokenExpr input = Prelude.map match
  where
    match tok = case lookupToken tok of
        Just idx -> From idx
        Nothing -> Literal tok
    lookupToken tok = elemIndex tok input

applyTokenExpr :: [TokenExpr] -> [T.Text] -> Maybe [T.Text]
applyTokenExpr exprs input = mapM apply exprs
  where
    apply (From i) =
        if i < length input
            then Just $ input !! i
            else Nothing
    apply (Literal t) = Just t

transformSelected :: EventM Name AppState ()
transformSelected = enterTransform

endTransformation :: EventM Name AppState ()
endTransformation = do
    range <- use selectionRange
    buf <- use bsBuffer
    let (lo, hi) = fromMaybe (0, 0) range
        contents = getEditContents buf
        selected = tailSafe $ take (hi - lo + 1) $ drop lo contents
        before = take lo contents
        after = drop (hi + 1) contents
        beforeTransformation = tokenize $ fromMaybe "" $ headMay $ drop lo contents
    s <- get
    case s ^? _TransformMode of
        Nothing -> enterVisual
        Just (_, _, transformEditor) ->
            let afterTransformation = tokenize $ T.concat $ getEditContents transformEditor
                tokenExpr = inferTokenExpr beforeTransformation afterTransformation
                changed = map (fmap (T.intercalate " ") . applyTokenExpr tokenExpr . tokenize) selected
                changed' = zipWith (\sel c -> if isNothing c then sel else fromMaybe "" c) selected changed
             in if all isNothing changed
                    then
                        enterVisual
                    else do
                        changeBuffer
                        let newContents = before ++ getEditContents transformEditor ++ changed' ++ after
                            cursor = getCursorPosition buf
                        bsBuffer .= newEditor (T.intercalate "\n" newContents)
                        buffer <- use bsBuffer
                        bsBuffer .= applyEdit (Zipper.moveCursor cursor) buffer
                        enterNormal
