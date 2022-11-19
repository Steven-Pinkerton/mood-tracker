module Spec where

import Data.Maybe (fromJust)
import MoodTracker.Parser (Mood (Bad), MoodEntry (MoodEntry), moodParse, moodWhat, moodWhen, myParseTimeM)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = do
  file <- readFileLBS "tests/test.jsonl"
  hspec $ do
    describe "Main" $ do
      it "moodParse works" $ do
        moodParse file
          `shouldBe` Right
            [MoodEntry {moodWhen = fromJust $ myParseTimeM "2022-10-09 Mon 11:51", moodWhat = Bad}]
