module Kikhteal.Bob where

import Data.Char(isSpace, isAlpha, isUpper)

responseFor::String -> String
responseFor phrase
        | all isSpace phrase = "Fine. Be that way!"
        | isCapitalized phrase = "Woah, chill out!"
        | last phrase == '?' = "Sure."
        | otherwise = "Whatever."
        where isCapitalized s = any isAlpha s && all (\c -> not (isAlpha c) || isUpper c) s