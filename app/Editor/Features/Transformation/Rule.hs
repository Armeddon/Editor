module Editor.Features.Transformation.Rule where

import qualified Data.Text as T

data TokenExpr = From Int | Literal T.Text deriving (Eq, Show)

type TransformationRule = [(TokenExpr, T.Text)]
