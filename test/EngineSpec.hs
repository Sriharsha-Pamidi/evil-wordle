module EngineSpec where

import Data.Time
import Engine
import Test.Hspec

spec :: Spec
spec = do
  describe "guess" $ do
    let test g t gts =
          shouldBe
            (map snd $ guess g t)
            gts
    it "abc -> abc" $ do
      test "abc" "abc" [CorrectSpot, CorrectSpot, CorrectSpot]
    it "aac -> abc" $ do
      test "aac" "abc" [CorrectSpot, NotInWord, CorrectSpot]
    it "abcb -> abcc" $ do
      test "abcd" "abcc" [CorrectSpot, CorrectSpot, CorrectSpot, NotInWord]
    it "correct chars" $ do
      map fst (guess "allow" "skill") `shouldBe` "allow"
    it "fever -> skill" $ do
      test "fever" "skill" [NotInWord, NotInWord, NotInWord, NotInWord, NotInWord]
    it "allow -> skill" $ do
      test "allow" "skill" [NotInWord, WrongSpot, WrongSpot, NotInWord, NotInWord]
    it "swiss -> skill" $ do
      test "swiss" "skill" [CorrectSpot, NotInWord, CorrectSpot, NotInWord, NotInWord]
    it "issue -> skill" $ do
      test "issue" "skill" [WrongSpot, WrongSpot, NotInWord, NotInWord, NotInWord]
    it "skill -> skill" $ do
      test "skill" "skill" [CorrectSpot, CorrectSpot, CorrectSpot, CorrectSpot, CorrectSpot]
    it "reels -> myers" $ do
      test "reels" "myers" [WrongSpot, NotInWord, CorrectSpot, NotInWord, CorrectSpot]
    it "seers -> myers" $ do
      test "reels" "myers" [WrongSpot, NotInWord, CorrectSpot, NotInWord, CorrectSpot]
    it "train -> timer" $ do
      test "train" "timer" [CorrectSpot, WrongSpot, NotInWord, WrongSpot, NotInWord]
    it "trine -> timer" $ do
      test "trine" "timer" [CorrectSpot, WrongSpot, WrongSpot, NotInWord, WrongSpot]
    it "their -> timer" $ do
      test "their" "timer" [CorrectSpot, NotInWord, WrongSpot, WrongSpot, CorrectSpot]
  describe "dailyWordIndex" $ do
    let test timeStr idx = do
          act <- dailyWordIndex $ parseTimeOrError True defaultTimeLocale "%FT%T" timeStr
          shouldBe act idx
    it "2022-01-01T00:00:00" $ do
      test "2022-01-01T00:00:00" 195
      test "2022-01-01T00:01:00" 195
      test "2022-01-01T23:59:59" 195
    it "2022-01-02T00:00:00" $ do
      test "2022-01-02T00:00:00" 196
      test "2022-01-02T00:01:00" 196
      test "2022-01-02T23:59:59" 196
    it "2022-01-02T00:00:00" $ do
      test "2022-02-10T00:00:00" 235
  describe "pickDailyWord" $ do
    let test timeStr w = do
          dict <- words <$> readFile "resource/dict/official.txt"
          let day = parseTimeOrError True defaultTimeLocale "%FT%T" timeStr
          word <- pickDailyWord day dict
          shouldBe word w
    it "2022-01-01T00:00:00" $ do
      test "2022-01-01T00:00:00" "rebus"
      test "2022-01-01T00:01:00" "rebus"
      test "2022-01-01T23:59:59" "rebus"
    it "2022-01-02T00:00:00" $ do
      test "2022-01-02T00:00:00" "boost"
      test "2022-01-02T00:01:00" "boost"
      test "2022-01-02T23:59:59" "boost"
    it "2022-01-02T00:00:00" $ do
      test "2022-02-10T00:00:00" "pause"
  describe "checkPos" $ do
    let test s listPos = do
          shouldBe (checkPos s) listPos
    it "a&b&c" $ do
      test "a&b&c" [1, 3]
    it "a&b&c&" $ do
      test "a&b&c&" [1, 3, 5]
    it "&a&b&c" $ do
      test "&a&b&c" [0, 2, 4]
  describe "checkNotPos" $ do
    let test s listPos = do
          shouldBe (checkNotPos s) listPos
    it "a&b&c" $ do
      test "a&b&c" [0, 2, 4]
    it "a&b&c&" $ do
      test "a&b&c&" [0, 2, 4]
    it "&a&b&c" $ do
      test "&a&b&c" [1, 3, 5]
  describe "matchAtPos" $ do
    let test inputList c pos matchList = do
          shouldBe (matchAtPos inputList c pos) matchList
    it "return all strings which match with given character at said position" $ do
      test ["abc", "bcd", "cde"] 'a' 0 ["abc"]
      test ["abc", "bcd", "cde"] 'b' 1 ["abc"]
      test ["abc", "bcd", "cde"] 'c' 0 ["cde"]
      test ["abc", "bcd", "cde"] 'a' 1 []
      test ["abc", "bcd", "cde"] 'd' 0 []
      test ["abc", "acd", "bde", "ad", "a"] 'a' 0 ["abc", "acd", "ad", "a"]
  describe "zeroMatch" $ do
    let test tupleList isZeroMatch = do
          shouldBe (zeroMatch tupleList) isZeroMatch
    it "return true only if none of the tuples have same values" $ do
      test [('a', 'b'), ('b', 'c'), ('c', 'd')] True
      test [('a', 'b'), ('b', 'c'), ('c', 'c')] False
      test [('a', 'b'), ('b', 'b'), ('c', 'd')] False
      test [('a', 'a'), ('b', 'b'), ('c', 'c')] False
      test [('a', 'b'), ('b', 'd'), ('c', 'e'), ('b', 'x'), ('f', 'z')] True
      test [('a', 'b'), ('b', 'd'), ('c', 'e'), ('b', 'x'), ('f', 'z'), ('a', 'a')] False
  describe "zeroMatchList" $ do
    let test inputList matchWord posList outputList = do
          shouldBe (zeroMatchList inputList matchWord posList) outputList
    it "return all strings which don't match with given string at input positions" $ do
      test ["light", "fight", "night", "sight"] "right" [0] ["light", "fight", "night", "sight"]
      test ["light", "fight", "night", "sight"] "light" [0] ["fight", "night", "sight"]
      test ["abc", "bcd", "dbe"] "abc" [0, 1] ["bcd"]
      test ["abc", "bcd", "ace", "cbd"] "abd" [0, 2] []
  describe "removeElement" $ do
    let test inputList element outputList = do
          shouldBe (removeElement inputList element) outputList
    it "remove element from list" $ do
      test ["1", "2", "3", "4", "5"] "3" ["1", "2", "4", "5"]
      test ["1", "2", "3", "4", "5"] "1" ["2", "3", "4", "5"]
      test ["5"] "5" []
  describe "removeElements" $ do
    let test inputList elements outputList = do
          shouldBe (removeElements inputList elements) outputList
    it "remove elements from list" $ do
      test ["1", "2", "3", "4", "5"] ["3", "4"] ["1", "2", "5"]
      test ["1", "2", "3", "4", "5"] ["1", "2"] ["3", "4", "5"]
      test ["5"] ["5"] []
  describe "getPosWord" $ do
    let test s1 s2 output = do
          shouldBe (getPosWord s1 s2) output
    it "return all tuples - position, char which match in the 2 given strings" $ do
      test "abc" "abc" [(0, 'a'), (1, 'b'), (2, 'c')]
      test "abc" "abd" [(0, 'a'), (1, 'b')]
      test "abc" "adc" [(0, 'a'), (2, 'c')]
      test "abc" "def" []
    describe "checkSanity" $ do
      let test s1 s2 isSame = do
            shouldBe (checkSanity s1 s2) isSame
      it "return true if for all positions in s1, s1 !! x is & or s1 !!x == s2 !! x" $ do
        test "abc" "abc" True
        test "abc" "abd" False
        test "abc" "adc" False
        test "abc" "a&c" True
    describe "checkSanityList" $ do
      let test inputList matchPattern outputList = do
            shouldBe (checkSanityList inputList matchPattern) outputList
      it "return all strings which match with given pattern" $ do
        test ["abc", "abd", "adc", "def"] "ab&" ["abc", "abd"]
        test ["abc", "abd", "adc", "def"] "a&c" ["abc", "adc"]
        test ["abc", "abd", "adc", "def"] "a&f" []
      