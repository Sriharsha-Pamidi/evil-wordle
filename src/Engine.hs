{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Engine where

import Data.List (intercalate, elemIndex)
import Data.Time
import System.Random
import Brick
import qualified Brick as T
import Data.Maybe (fromJust)
import GHC.IO (unsafePerformIO)

data GuessType = Default | NotInWord | WrongSpot | CorrectSpot
  deriving (Show, Read, Eq, Ord)

type Guess = [(Char, GuessType)]

data GameStatus = Win | Loss | Ongoing
  deriving (Show, Read, Eq, Ord)

data GameMode = Daily | Infinite
  deriving (Show, Read, Eq, Ord)

--Function to read the lines in the file 
readWords :: FilePath -> IO [String]
readWords p = lines <$> readFile p

-- Pick a word
pickWord :: [String] -> IO String
pickWord = pickWordFilter $ const True

-- pick a word from the list of strings based on the filter
pickWordFilter :: (String -> Bool) -> [String] -> IO String
pickWordFilter f ws = do
  let fws = filter f ws
  n <- randomRIO (0, length fws - 1)
  return $ fws !! n


pickDailyWord :: Day -> [String] -> IO String
pickDailyWord day dict = do
  ix <- dailyWordIndex day
  return $ dict !! (ix `mod` length dict)

dailyWordIndex :: Day -> IO Int
dailyWordIndex currentDay = do
  -- January 1, 2022 Game Epoch
  let startDay = parseTimeOrError True defaultTimeLocale "%F" "2022-01-01"
  let sinceDays = diffDays currentDay startDay
  return . fromIntegral . (+ 195) $ sinceDays

isCorrectWord :: String -> [String] -> Bool
isCorrectWord = elem

guess :: String -> String -> Guess
guess gWord tWord = foldl f [] $ zip gWord tWord
  where
    f :: Guess -> (Char, Char) -> Guess
    f acc (g, t) = acc ++ [(g, guessType)]
      where
        guessType
          | g == t = CorrectSpot
          | targetTotal g - correctTotal g - wrongBefore g > 0 = WrongSpot
          | otherwise = NotInWord
        wrongBefore c = length . filter (== c) . map fst . filter ((== WrongSpot) . snd) $ acc
        targetTotal c = length . filter (== c) $ tWord
        correctTotal c = length . filter (\(a, b) -> a == c && b == c) $ zip gWord tWord


--removeWords wlist word threshold = foldl f [] wlist word threshold 
removeWords :: [String] -> String -> Int -> [String]
removeWords [] word threshold = []
removeWords (x:xs) word threshold = f x word threshold ++ removeWords xs word threshold
  where
    f :: String -> String -> Int -> [String]
    f w guess thresh = x
        where
          x
            | scoreCompare w guess > thresh = []
            | otherwise = [w]

-- generate score based on comparison 
scoreList :: [String] -> String -> [Int]
scoreList wordlist gword = map (scoreCompare gword) wordlist

-- threshold will be the minimum score in the list of scores 
getThreshold :: [String] -> String -> Int
getThreshold wordlist gword = minimum (scoreList wordlist gword)

-- compare the words to get the score 
scoreCompare :: String -> String -> Int
scoreCompare gWord tWord = guessScore1 (guessScore gWord tWord)
--   where
--     f :: Int -> (Char, Char) -> Int
--     f acc (g, t) = acc + guessScore
--       where
--         guessScore
--           | g == t = 2
--           | targetTotal g - correctTotal g - wrongBefore g > 0 = 1
--           | otherwise = 0
--         wrongBefore c = length . filter (== c) . map fst . filter ((== WrongSpot) . snd)
--         targetTotal c = length . filter (== c) $ tWord
--         correctTotal c = length . filter (\(a, b) -> a == c && b == c) $ zip gWord tWord
--     n = 0


guessScore1 :: [(Char, Int)] -> Int
guessScore1 [] = 0
guessScore1 (x:xs) = f x + guessScore1 xs
  where
    f :: (Char, Int) -> Int
    f (_, x) = x

guessScore :: String -> String -> [(Char, Int)]
guessScore gWord tWord = foldl f [] $ zip gWord tWord
  where
    f :: [(Char, Int)] -> (Char, Char) -> [(Char, Int)]
    f acc (g, t) = acc ++ [(g, guessType)]
      where
        guessType
          | g == t = 2
          | targetTotal g - correctTotal g - wrongBefore g > 0 = 1
          | otherwise = 0
        wrongBefore c = length . filter (== c) . map fst . filter ((== 1) . snd) $ acc
        targetTotal c = length . filter (== c) $ tWord
        correctTotal c = length . filter (\(a, b) -> a == c && b == c) $ zip gWord tWord


-- match at particular position 
matchAtPos :: [String] -> Char -> Int -> [String]
matchAtPos [] c p = []
matchAtPos (x:xs) c p
                    | (x !! p) == c = x:matchAtPos xs c p
                    | otherwise = matchAtPos xs c p

-- match at any position
matchAtAnyPos :: [String] -> Char -> Int -> [String]
matchAtAnyPos wordlist c 0 = []
matchAtAnyPos wordlist c n = matchAtPos wordlist c (n-1) ++ matchAtAnyPos wordlist c (n-1)

-- no matches 
zeroMatch :: [(Char, Char)] -> Bool
zeroMatch [] = True
zeroMatch (x:xs) = uncurry (/=) x  && zeroMatch xs


-- remove a particular element from the list 
removeElement :: [String] -> String -> [String]
removeElement [] w1 = []
removeElement (x:xs) w1 = if x == w1 then l1 else x:l1
  where
    l1 = removeElement xs w1

-- to perform the operation list1-list2 
removeElements :: [String] -> [String] -> [String]
removeElements = foldl removeElement

-- get the list with no elements matching 
getNonMatchingList :: [String] -> [(Int, [String])] -> [String]
getNonMatchingList = foldl
      (\ wordlist x -> removeElements wordlist (snd x))



zeroMatchList :: [String] -> String -> [Int] -> [String]
zeroMatchList [] s pList = []
zeroMatchList (x:xs) s pList
                      | zeroMatch compList = x: zeroMatchList xs s pList
                      | otherwise = zeroMatchList xs s pList
                      where
                        compList = [(x !! p, s !! p) | p <- pList ]

checkPos :: String -> [Int]
checkPos s1 = [x | x <- [0..(length s1 -1)], s1 !! x == '&']

checkNotPos :: String -> [Int]
checkNotPos s1 = [x | x <- [0..(length s1 -1)], s1 !! x /= '&']

-- position and the corresponding match list 
getPosLists :: [String] -> String -> String -> Bool -> [(Int, [String])]
getPosLists wordList refWord dispWord withNonMatching = if withNonMatching then list1 ++ list2 else list1
  where
    list1 = [(p, matchAtPos wordList (refWord !! p) p) | p <- checkPos dispWord] -- matching at some position
    list2 = [(length refWord, zeroMatchList wordList refWord (checkNotPos dispWord))] -- non matching words

getPosLists2 :: [String] -> String -> String -> Bool -> [(Int, [String])]
getPosLists2 wordList refWord dispWord withNonMatching = if withNonMatching then list1 ++ list2 else list1
  where
    list1 = [(p, matchAtAnyPos wordList (refWord !! p) (length refWord)) | p <- checkPos dispWord] -- matching at some position
    list2 = [(length refWord, getNonMatchingList wordList list1)] -- non matching words

-- position with max number of possibilities 
getMaxPos :: [(Int, [String])] -> (Int, [String])
getMaxPos posLists = posLists !! n
  where
    freqList = [length (snd x) | x <- posLists]
    maxFreq = maximum freqList
    Just n = elemIndex maxFreq freqList


getPosWord :: String -> String -> [(Int, Char)]
getPosWord refWord randWord = [(x, refWord !! x) | x <- [0..(length refWord -1)], refWord !! x == randWord !! x]

-- update the match string 
updateDispWord1 :: String -> [(Int, Char)] -> String
updateDispWord1 dispWord [] = dispWord
updateDispWord1 dispWord (x:xs) = updateDispWord1 (l1 ++ [snd x] ++ l2) xs
  where
    l1 = take (fst x) dispWord
    l2 = drop (fst x +1) dispWord

updateDispWord :: String -> [(Int, Char)] -> String
updateDispWord dispWord charList = [uncurry dispHelp x | x <- zip dispWord upWord]
  where
    upWord = updateDispWord1 dispWord charList

dispHelp :: Char -> Char -> Char
dispHelp c1 c2 = if c1 /= '&' then c1 else c2

checkSanity :: String -> String -> Bool
checkSanity [] [] = True
checkSanity (x:w1) (y:w2) = res && checkSanity w1 w2
  where
    res = (y == '&') || (x == y)

checkSanityList :: [String] -> String -> [String]
checkSanityList [] dispWord = []
checkSanityList (x:xs) dispWord = y ++ checkSanityList xs dispWord
  where
    y = [x | checkSanity x dispWord]

showResultGrid :: Bool -> [Guess] -> String
showResultGrid isUnicode = intercalate "\n" . map (concatMap (showGuessTypeAscii . snd))

showGuessTypeAscii :: GuessType -> String
showGuessTypeAscii s = case s of
  Default -> " "
  NotInWord -> "."
  WrongSpot -> "w"
  CorrectSpot -> "c"

showEvilGrid :: Int -> [Char]
showEvilGrid s = intercalate "\n" [showEvilTypeUnicode s]

showEvilTypeAscii :: Int -> String
showEvilTypeAscii s = case s of
  0 -> " "
  1 -> "."
  2 -> "Evil"
  3 -> "...."

showEvilTypeUnicode :: Int -> String
showEvilTypeUnicode s = case s of
  0 -> " "
  1 -> "👽"
  2 -> "👺"
  3 -> "👽"



-- write wordlist to file
writeWordList :: FilePath -> [String] -> IO ()
writeWordList path wordlist = do
  writeFile path (unlines wordlist)

dumpPath = "resource/dict/dump.txt"


generateEmpty :: Int -> [T.Widget str]
generateEmpty 0 = []
generateEmpty n = str (showEvilGrid 1) : generateEmpty (n-1)

generateEvil :: Int -> Int -> T.Widget str -> [T.Widget str]
generateEvil n m s = take a emptyList ++ [s] ++ drop (a+1) emptyList
  where
    emptyList = generateEmpty m
    a = n `mod` m


-- function which generates random number
getRandom :: Int -> IO Int
getRandom n = getStdRandom (randomR (0, n))

-- select random word from list of words
selectRandomWord :: [String] -> String
selectRandomWord wordlist = wordlist !! n
  where
    n = unsafePerformIO (getRandom (length wordlist -1))


isElementOf :: String -> [String] -> Bool
isElementOf s1 [] = False
isElementOf s1 (x:xs) = (x == s1) || isElementOf s1 xs
