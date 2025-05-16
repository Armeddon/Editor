module Editor.Model.Mode where

import Data.Char (toUpper)

data Mode = Normal | Insert | Visual | Open | Transform deriving (Eq, Show)

modeString :: Mode -> String
modeString = map toUpper . show
