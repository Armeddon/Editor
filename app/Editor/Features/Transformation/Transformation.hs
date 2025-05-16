{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Editor.Features.Transformation.Transformation (transformSelected, endTransformation, repeatLastTransformation) where

import Brick
import Brick.Widgets.Edit (applyEdit, getCursorPosition, getEditContents)
import Control.Lens
import Data.Char (isSpace)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Zipper
import Editor.Features.Transformation.Rule
import Editor.Model
import Editor.Model.Name
import Editor.Model.State
import Safe (headMay, tailSafe)

tokenizeWithSpaces :: T.Text -> [(T.Text, T.Text)]
tokenizeWithSpaces t
    | T.null t = []
    | otherwise =
        let (sp, rst) = T.span isSpace t
            (tok, rest) = T.break isSpace rst
            (ws, rest') = T.span isSpace rest
            subtoks = splitBoundary tok
            tokens = case reverse subtoks of
                [] -> []
                (lastTok : restRev) -> reverse (map (,"") restRev) ++ [(lastTok, ws)]
         in [("", sp) | not $ T.null sp] ++ tokens ++ tokenizeWithSpaces rest'

tokenize :: T.Text -> [T.Text]
tokenize t = do
    ws <- T.words t
    splitBoundary ws

splitBoundary :: T.Text -> [T.Text]
splitBoundary = T.groupBy keepGrouped
  where
    keepGrouped a b = not (isBoundaryChar a || isBoundaryChar b)
    isBoundaryChar c = c `elem` ("()[]{},;/+-*" :: String)

inferTokenExpr :: [T.Text] -> [(T.Text, T.Text)] -> TransformationRule
inferTokenExpr input = Prelude.map match
  where
    match (tok, s) = case lookupToken tok of
        Just idx -> (From idx, s)
        Nothing -> (Literal tok, s)
    lookupToken tok = elemIndex tok input

applyTokenExpr :: TransformationRule -> [T.Text] -> Maybe [(T.Text, T.Text)]
applyTokenExpr exprs input = mapM apply exprs
  where
    apply (From i, s) =
        if i < length input
            then Just (input !! i, s)
            else Nothing
    apply (Literal t, s) = Just (t, s)

applyTransformation :: TransformationRule -> Maybe T.Text -> EventM Name AppState ()
applyTransformation rule firstLine = do
    range <- use selectionRange
    buf <- use bsBuffer
    let (lo, hi) = fromMaybe (0, 0) range
        contents = getEditContents buf
        selectedAll = take (hi - lo + 1) $ drop lo contents
        selected = if isNothing firstLine then selectedAll else tailSafe selectedAll
        before = take lo contents
        after = drop (hi + 1) contents
        changed = map (fmap (T.concat . map (uncurry T.append)) . applyTokenExpr rule . tokenize) selected
        changed' = zipWith fromMaybe selected changed
     in if all isNothing $ firstLine : changed
            then
                enterVisual
            else do
                changeBuffer
                let newContents = before ++ fromMaybe [] (sequence [firstLine]) ++ changed' ++ after
                    cursor = getCursorPosition buf
                bsBuffer .= newEditor (T.intercalate "\n" newContents)
                use bsBuffer
                    >>= (bsBuffer .=) . applyEdit (Zipper.moveCursor cursor)
                bsMessage .= "Rule applied to " ++ show (length changed') ++ " lines"
                bsLastTransformation .= Just rule
                enterNormal

transformSelected :: EventM Name AppState ()
transformSelected = enterTransform

repeatLastTransformation :: EventM Name AppState ()
repeatLastTransformation = do
    lastTransformation <- use bsLastTransformation
    applyTransformation (fromMaybe [] lastTransformation) Nothing

endTransformation :: EventM Name AppState ()
endTransformation = do
    s <- get
    range <- use selectionRange
    buf <- use bsBuffer
    let (lo, _) = fromMaybe (0, 0) range
        contents = getEditContents buf
        beforeTransformation = tokenize $ fromMaybe "" $ headMay $ drop lo contents
    case s ^? _TransformMode of
        Nothing -> enterVisual
        Just (_, _, transformEditor) ->
            let afterTransformation = tokenizeWithSpaces $ T.concat $ getEditContents transformEditor
                tokenExpr = inferTokenExpr beforeTransformation afterTransformation
             in applyTransformation tokenExpr (Just $ T.concat $ getEditContents transformEditor)
