{-# LANGUAGE QuasiQuotes #-}

module Spec where

import Data.ByteString.Lazy ()
import Data.Maybe (fromJust)
import MoodTracker.Parser (Mood (Bad, Good), MoodEntry (MoodEntry), moodParse, moodWhat, moodWhen, myParseTimeM)
import NeatInterpolation (trimming)
import Test.Hspec (describe, hspec, it, shouldBe)

--This provides the text for the test.
testText :: Text
testText =
  [trimming|
      {"mood": "Bad", "when": "2022-10-09 Mon 11:51"}
      {"mood": "Good", "when": "2022-11-02 Sat 09:10"}|]

main :: IO ()
main = do
  let file = fromString "{\"mood\": \"Bad\", \"when\": \"2022-10-09 Mon 11:51\"}\n{\"mood\": \"Good\", \"when\": \"2022-11-02 Sat 09:10\"}"
  hspec $ do
    describe "Main" $ do
      it "moodParse works" $ do
        moodParse file
          `shouldBe` Right
            [MoodEntry {moodWhen = fromJust $ myParseTimeM "2022-10-09 Mon 11:51", moodWhat = Bad}, MoodEntry {moodWhen = fromJust $ myParseTimeM "2022-11-02 Sat 09:10", moodWhat = Good}]
