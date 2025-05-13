{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Transformation (transformSelected, endTransformation) where

import Brick
import Brick.Widgets.Edit (applyEdit, getCursorPosition, getEditContents)
import Control.Lens
import Data.Char (isSpace)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Zipper
import Safe (headMay, tailSafe)
import State

tokenizeWithSpaces :: T.Text -> [(T.Text, T.Text)]
tokenizeWithSpaces t
    | T.null t = []
    | otherwise =
        let (tok, rest) = T.break isSpace t
            (ws, rest') = T.span isSpace rest
            subtoks = splitBoundary tok
            tokens = case reverse subtoks of
                [] -> []
                (lastTok : restRev) -> reverse (map (,"") restRev) ++ [(lastTok, ws)]
         in tokens ++ tokenizeWithSpaces rest'

tokenize :: T.Text -> [T.Text]
tokenize t = T.words t >>= splitBoundary

splitBoundary :: T.Text -> [T.Text]
splitBoundary = T.groupBy keepGrouped
  where
    keepGrouped a b = not (isBoundaryChar a || isBoundaryChar b)
    isBoundaryChar c = c `elem` ("()[]{},;" :: String)

data TokenExpr = From Int | Literal T.Text deriving (Eq, Show)

inferTokenExpr :: [T.Text] -> [(T.Text, T.Text)] -> [(TokenExpr, T.Text)]
inferTokenExpr input = Prelude.map match
  where
    match (tok, s) = case lookupToken tok of
        Just idx -> (From idx, s)
        Nothing -> (Literal tok, s)
    lookupToken tok = elemIndex tok input

applyTokenExpr :: [(TokenExpr, T.Text)] -> [T.Text] -> Maybe [(T.Text, T.Text)]
applyTokenExpr exprs input = mapM apply exprs
  where
    apply (From i, s) =
        if i < length input
            then Just (input !! i, s)
            else Nothing
    apply (Literal t, s) = Just (t, s)

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
            let afterTransformation = tokenizeWithSpaces $ T.concat $ getEditContents transformEditor
                tokenExpr = inferTokenExpr beforeTransformation afterTransformation
                changed = map (fmap (T.concat . map (uncurry T.append)) . applyTokenExpr tokenExpr . tokenize) selected
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
