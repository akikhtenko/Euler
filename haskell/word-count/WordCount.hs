module WordCount where

import Data.Char (toLower, isAlphaNum)
import Data.Function (on)
import Data.List (groupBy)
import Data.Map (Map, fromListWith)

wordCount::String -> Map String Int
wordCount text =
    let
        lowerText = map toLower text
        ungroupedWords = zip (words' lowerText) $ repeat 1
    in fromListWith (+) ungroupedWords

words'::String -> [String]
words' = everyOdd . splitByAlphaNum . trimPunctuation
    where
        trimPunctuation = dropWhile (not.isAlphaNum)
        splitByAlphaNum = groupBy ((==) `on` isAlphaNum)

everyOdd::[a] -> [a]
everyOdd xs = map head $ splitAtEvery 2 xs

splitAtEvery::Int -> [a] -> [[a]]
splitAtEvery _ [] = []
splitAtEvery n xs = let (x1, x2) = splitAt n xs
    in x1:(splitAtEvery n x2)