module Scanner (scanTokens) where

import Data.Maybe
import Data.Char

import Tokens

scanTokens :: [Char] -> [Token]
scanTokens str = 