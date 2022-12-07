{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module App where

import qualified Brick.AttrMap as A
import qualified Brick.Main as BM
import qualified Brick.Types as T
import Brick.Util (fg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core
import Cmd
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time
import Engine
import Graphics.Vty as V hiding (Default)
import Text.Printf (printf)
import System.Random
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import GHC.Conc (threadDelay)
import Brick (customMain)
import Data.Functor (void)

data Tick = Tick

data State = State
  { sWords :: [String],
    sWord :: String,
    sWordSize :: Int,
    sGuesses :: [Guess],
    sPossible :: [String],
    sMaxGuesses :: Int,
    sInput :: String,
    sStatus :: String,
    sGameStatus :: GameStatus,
    sGameMode :: GameMode,
    sResults :: [String],
    sUnicode :: Bool,
    sWordIx :: String,
    sEvilPos :: Int,
    sGuessWords :: [String]
  }
  deriving (Show, Read, Eq, Ord)

initState :: IO State
initState = do
  ss <- configSettings
  let gm = Infinite
      showIx = "∞"
  word <- pickWordFilter ((== wordSize ss) . length) . targetDict $ ss
  return $
    State
      { sWords = guessDict ss,
        sWord = "&&&&&",
        sWordSize = wordSize ss,
        sGuesses = [],
        sPossible = filter ((== wordSize ss) . length) . guessDict $ ss,
        sMaxGuesses = maxGuesses ss,
        sInput = "",
        sStatus = "Wordle " ++ showIx,
        sGameStatus = Ongoing,
        sGameMode = gm,
        sResults = [],
        sUnicode = unicode ss,
        sWordIx = showIx,
        sEvilPos = 0,
        sGuessWords = []
      }

appMain :: IO State
appMain = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  --BM.defaultMain app =<< initState
  g <- initState
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  customMain initialVty builder (Just chan) app g

app :: BM.App State Tick ()
app =
  BM.App
    { BM.appDraw = draw,
      BM.appChooseCursor = BM.showFirstCursor,
      BM.appHandleEvent = handleEvent,
      BM.appStartEvent = return,
      BM.appAttrMap = const attrMap
    }

drawUI :: State -> [T.Widget n]
drawUI s = [center $ padRight (T.Pad 2) (drawGrid s) <+> drawStats s ]

drawStats :: State -> T.Widget n
drawStats = undefined

drawGrid :: State -> T.Widget n
drawGrid = undefined

draw :: State -> [T.Widget n]
draw s =
  [ center . vLimit guessHeight . vBox $
      [ hBox
          [ padLeft (T.Pad 1) . padRight (T.Pad 2) . padBottom T.Max . hLimit 10 . hCenter . vBox . map hCenter $
              generateEvil (sEvilPos s) 50 (str evilString),
            str "  ",
            padLeft (T.Pad 1) . padRight (T.Pad 2) . padBottom T.Max . hLimit guessWidth . hCenter . vBox . map hCenter $
              [ str "Evil Wordle",
                str "  ",
                str " Wordle is a word guessing ",
                str " game. Evil Wordle is the ",
                str " evil twin of the wordle.",
                str " Everytime you guess, The ",
                str " answer word changes! ",
                str " Try not to GIVE UP..!!",
                str "  ", 
                str "Input",
                drawInput,
                str "  ", 
                str "Guesses",
                drawGuesses (sGuesses s),
                drawGuesses futureGuesses,
                str "  ",
                -- vLimit 1 . fill $ ' ',
                status,
                str "  "
              ],
            str "  ",
            padLeft (T.Pad 1) . padRight (T.Pad 2) . padBottom T.Max . hLimit 10 . hCenter . vBox . map hCenter $
              generateEvil (sEvilPos s + 10) 50 (str evilString)
          ]
      ]
  ]
  where
    guessWidth = 5 * sWordSize s + 1
    guessHeight = maximum [17, 3 * sMaxGuesses s + 3] + 25
    status = withAttr wrongSpotAttr . padLeft (T.Pad 1) . strWrap . sStatus $ s
    drawGuesses = vBox . map drawGuess
    drawGuess = hBox . map drawChar
    drawChar :: (Char, GuessType) -> T.Widget n
    drawChar c = charAttr (snd c) . border . padRight (T.Pad 1) . padLeft (T.Pad 1) . str . (: []) . fst $ c
    charAttr gt = case gt of
      NotInWord -> withAttr notInWordAttr
      WrongSpot -> withAttr secondaryTextAttr
      CorrectSpot -> withAttr correctSpotAttr
      Default -> id
    futureGuesses = map (const futureGuess) [0 .. sMaxGuesses s - length (sGuesses s) - 1]
    futureGuess = map (const (' ', Default)) [0 .. sWordSize s - 1]
    drawInput = padLeft (T.Pad 1) . drawGuess . map (,Default) . (\cs -> cs ++ replicate (sWordSize s - length cs) ' ') . sInput $ s
    guessedMap = M.fromListWith max . nub . concat . sGuesses $ s
    evilString = showEvilGrid 2

handleEvent :: State -> T.BrickEvent n e -> T.EventM n (T.Next State)
handleEvent s e = case sGameStatus s of
  Ongoing -> case e of
    T.VtyEvent ve -> case ve of
      V.EvKey (V.KChar 'c') [V.MCtrl] -> BM.halt s
      V.EvKey (V.KChar k) [] -> inputH k
      V.EvKey V.KBS [] -> bspcH
      V.EvKey V.KEnter [] -> guessH
      _ -> animateH
    _ -> animateH
    where
      clearH = BM.continue $ s {sInput = ""}
      giveUpH = BM.continue $ s {sGameStatus = Loss, sStatus = printf "You lose, the word was \"%s\"" randWord}
      inputH k = BM.continue $ s {sInput = take (sWordSize s) $ sInput s ++ [k]}
      bspcH = BM.continue $ s {sInput = if null $ sInput s then "" else init $ sInput s}
      guessH = do
        let ns
              | length g /= length w = s {sStatus = printf "Word size must be %d" $ length w}
              | not $ isCorrectWord g ws = s {sStatus = printf "\"%s\" is not a valid word" g}
              | isElementOf g gList = s {sStatus = printf "Repeated guess word \"%s\" " g}
              | otherwise =
                case (g == newWord, length (sGuesses s') == sMaxGuesses s') of
                  (True, _) ->
                    s'
                      { sGameStatus = Win,
                        sStatus = "You guessed the word!",
                        sResults = sResults s' ++ [showResult s']
                      }
                  (_, True) ->
                    s'
                      { sGameStatus = Loss,
                        sStatus = printf "Wrong guess, the word was \"%s\"" randHead
                      }
                  (_, False) -> s' {sStatus = printf "Updating possible word list",
                                -- sStatus = printf "Updating possible word list %d sanList %d %s %s %s sanity %s" (length newList) (length sanList) dispString dispHead dispW san,
                                    sPossible = newList,
                                    sWord = newWord,
                                    sGuessWords = sGuessWords s' ++ [g]
                                    }
              where
                s' = s {sGuesses = sGuesses s ++ [guess g newWord], sInput = ""}
                showResult _s = intercalate "\n" (t : "" : grid)
                  where
                    t = unwords ["Wordle", n, att]
                    n = sWordIx _s
                    att = concat [show . length . sGuesses $ _s, "/", show . sMaxGuesses $ _s]
                    grid = sResults _s ++ [showResultGrid (sUnicode _s) . sGuesses $ _s]
                newList = if length tempList == 1 then tempList else removeElement tempList g
                tempList = updatePossible s'
                updatePossible _s = safeList
                  where
                    wordlist = sPossible _s
                    filterList = checkSanityList wordlist (sWord _s)
                    (refreshPos, refreshList) = getMaxPos (getPosLists filterList g w withNonMatching)
                    -- updatedList = if length wordlist == 1 then wordlist else refreshList
                    safeList = if null refreshList then [head wordlist] else refreshList
                    withNonMatching = True -- length (sGuesses s) < 3

                newWord = updateDispWord w (getPosWord g (head newList))
                dispString = show (getPosWord g (head newList))
                sanList = checkSanityList newList newWord
                dispHead = show (head newList)
                randHead = selectRandomWord newList
                dispW = show newWord
                ultiList = if not (null sanList) then sanList else [randHead]
                san = show (length sanList == length newList)  
                --writeWordList filePath (sPossible s)
        BM.continue ns {sInput = ""}
  _ -> case e of
    T.VtyEvent ve -> case ve of
      V.EvKey (V.KChar 'c') [V.MCtrl] -> BM.halt s
      V.EvKey (V.KChar 'r') [] -> BM.continue =<< liftIO initState
      _ -> animateH
    _ -> animateH
  where
    g = sInput s
    w = sWord s
    ws = sWords s
    gList = sGuessWords s
    attempt = guess g w
    randWord = head (sPossible s)
    --filePath = "resource/dict/dump_" ++ show (length (sGuesses s')) ++ ".txt"
    animateH = BM.continue $ s {sEvilPos = newPos} 
      where 
        newPos = (sEvilPos s + 1) `mod` 50

attrMap :: A.AttrMap
attrMap =
  A.attrMap
    V.defAttr
    [ (notInWordAttr, fg V.brightBlack),
      (wrongSpotAttr, fg V.yellow),
      (correctSpotAttr, fg V.green),
      (secondaryTextAttr, fg V.brightBlack)
    ]

notInWordAttr :: A.AttrName
notInWordAttr = "notInWord"

wrongSpotAttr :: A.AttrName
wrongSpotAttr = "wrongSpot"

correctSpotAttr :: A.AttrName
correctSpotAttr = "correctSpot"

secondaryTextAttr :: A.AttrName
secondaryTextAttr = "secondaryText"
