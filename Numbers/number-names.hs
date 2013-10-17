-- Haskell program to spell out positive integers in English
-- Supports integers n s.t. 1 <= n < 1 000 000

maxSupported = 1000000 - 1 :: Int
minSupported = 1 :: Int

spellNumber :: Int -> String
spellNumber n   
    | n < minSupported || n > maxSupported     = error "Supported range is "  
                                                    ++ (show minSupported) 
                                                    ++ " - " ++ (show maxSupported)
    | otherwise     =  punctuateList $ spNum n
    
spNum :: Int -> [String]
spNum n = spellHundredThousands (n `div` 1000) ++ spellThousands (n `mod` 1000)


-- expects a number in the range 1 - 999
-- adds the string " thousand " after if needed
spellHundredThousands :: Int -> [String]
spellHundredThousands 0 = []
spellHundredThousands n = [spellAppend (++" thousand") (punctuateList . spellThousands) n]

-- expects a number in the range 1 - 999
spellThousands :: Int -> [String]
spellThousands 0 = []
spellThousands n = spellHundreds (n `div` 100) ++ wrap (spell (n `mod` 100)) 

spellHundreds :: Int -> [String]
spellHundreds 0 = []
spellHundreds n = [spellAppend (++" hundred") spell n]

-- expects a number in the range 1 - 99
spell n | n < 20   = spellSmall n
        | otherwise = spellTens (n `div` 10) ++ spellAppend (" "++) spellSmall (n `mod` 10)

spellTens n = tensNumberNames !! (n - 2)

tensNumberNames = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

spellSmall n = smallNumberNames !! n

smallNumberNames  = ["", "one", "two", "three", "four", "five", "six", "seven"
                ,"eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen"
                ,"fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

spellAppend :: (String -> String) -> (Int -> String) -> Int -> String
spellAppend g f n = let str = f n
                    in case str of
                        ""  ->  ""
                        _   ->  g str 

punctuateList :: [String] -> String
punctuateList [] = ""
punctuateList [str] = str
punctuateList xs = go xs
    where
        go [x,y] = x ++ " and " ++ y
        go (x:xs) = x ++ ", " ++ go xs

wrap :: String -> [String]
wrap "" = []
wrap x = [x]
