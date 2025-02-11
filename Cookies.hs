-- The Most Over-Engineered Fortune Cookie 🥠 Generator Ever
-- Warning: Contains dad jokes and unnecessary complexity

import System.Random
import Data.List
import Control.Monad

-- Define our highly sophisticated types
newtype Wisdom = Wisdom String deriving Show
newtype DadJoke = DadJoke String deriving Show
data FortuneLevel = Meh | NotBad | Amazing | MindBlowing🤯 deriving (Show, Eq, Ord)

-- The most complex way possible to combine two strings
overEngineeredCombine :: String -> String -> String
overEngineeredCombine x y = foldr (++) "" $ intersperse " " $ filter (not . null) [x, y]

-- Because one function to generate random numbers is too mainstream
getComplicatedRandom :: Int -> Int -> IO Int
getComplicatedRandom min max = do
    x <- randomRIO (min, max)
    y <- randomRIO (min, max)
    return $ (`mod` (max - min + 1)) $ sum [x, y, abs (x - y)]

-- The world's most unnecessary function to capitalize a string
wayTooComplexCapitalize :: String -> String
wayTooComplexCapitalize [] = []
wayTooComplexCapitalize (x:xs) = 
    toEnum (fromEnum x + if fromEnum x >= 97 && fromEnum x <= 122 
                        then -32 
                        else 0) : xs

-- Fortune cookie wisdoms that will change your life... maybe
wisdoms :: [Wisdom]
wisdoms = map Wisdom
    [ "He who codes without coffee is bound to fall asleep"
    , "Your code will compile... eventually"
    , "The bug you seek is in the last place you look"
    , "Tonight you will dream of infinite recursion"
    , "You will soon encounter a wild segmentation fault"
    ]

-- Dad jokes that make you question your life choices
dadJokes :: [DadJoke]
dadJokes = map DadJoke
    [ "Why do programmers always mix up Halloween and Christmas? Because Oct 31 = Dec 25!"
    , "Why do Java developers wear glasses? Because they don't C#!"
    , "What's a programmer's favorite place? The Foo Bar!"
    , "Why did the functional programmer get thrown out of school? Because they refused to take classes!"
    , "What do you call a programmer from Finland? Nerdic!"
    ]

-- Determine how amazing this fortune is using SCIENCE!
calculateFortuneLevel :: String -> FortuneLevel
calculateFortuneLevel fortune = 
    case length fortune `mod` 4 of
        0 -> Meh
        1 -> NotBad
        2 -> Amazing
        _ -> MindBlowing🤯

-- The most elaborate way possible to pick a random element
pickRandom :: [a] -> IO a
pickRandom xs = do
    let len = length xs
    randomIndex <- replicateM 3 (getComplicatedRandom 0 (len - 1))
    return $ xs !! (sum randomIndex `mod` len)

-- Generate a fortune with maximum overengineering
generateFortune :: IO String
generateFortune = do
    Wisdom wisdom <- pickRandom wisdoms
    DadJoke joke <- pickRandom dadJokes
    let fortune = overEngineeredCombine wisdom joke
    let level = calculateFortuneLevel fortune
    return $ wayTooComplexCapitalize $ fortune ++ "\nFortune Level: " ++ show level

-- Main function that prints multiple fortunes because one is not enough
main :: IO ()
main = do
    putStrLn "=== The Over-Engineered Fortune Cookie Generator ==="
    putStrLn "Calculating fortunes using highly sophisticated algorithms...\n"
    replicateM_ 3 $ do
        fortune <- generateFortune
        putStrLn $ replicate 50 '='
        putStrLn fortune
        putStrLn $ replicate 50 '='
        putStrLn ""
