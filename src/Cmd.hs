module Cmd where

data Settings = Settings
  { 
    targetDict :: [String],
    guessDict :: [String],
    maxGuesses :: Int,
    wordSize :: Int,
    unicode :: Bool
  }
  deriving (Show, Read, Eq, Ord)

configSettings :: IO Settings
configSettings = do
  td <- words <$> readFile "resource/dict/en-10k.txt"
  gd <- words <$> readFile "resource/dict/en-84k.txt"
 
  let attempts = 7
      wordLength = 5
      settings =
        Settings
          { targetDict = td,
            guessDict = gd,
            maxGuesses = attempts,
            wordSize = wordLength,
            unicode = False
          }

  return settings
