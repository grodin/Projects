-- Program to input text (assumed to be English) and convert to pig-latin

module Main where

import Data.Char (isUpper, toUpper, toLower)

main = interact pigLatinize

pigLatinize :: String -> String
pigLatinize = (++ "\n") . unwords . map pigLatinizeWord . words

pigLatinizeWord :: String -> String
pigLatinizeWord s@(c:cs)    
    | isUpper c     =   upperCaseInitial . go $ toLower c : cs
    | otherwise     =   go s
    where
        go s' = tail ++ (applyRules init)
            where
                (init, tail)        =   splitToVowel s'
                applyRules []       =   "way"
                applyRules str      =   str ++ "ay"

splitToVowel :: String -> (String, String)
splitToVowel s@(c:cs)   =   (init, tail)
    where
        (init, tail) = case c of
            'y' ->  mapFst ("y" ++) $ splitToVowel cs
            _   ->  span (not . (`elem` vowelsY)) s

upperCaseInitial :: String -> String
upperCaseInitial (c:cs) = toUpper c : cs
upperCaseInitial _      = []  

vowels = "aeiouAEIOU"
vowelsY= vowels ++ "yY"

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x, y)
