{-# LANGUAGE QuasiQuotes #-}

module Spec where

import Data.Maybe (fromJust)
import Data.Time (defaultTimeLocale, parseTimeM)
import MoodTracker.Parser (Mood (Bad, Good), MoodEntry (MoodEntry), moodWhat, moodWhen, parseMoodEntries)
import NeatInterpolation (trimming)
import Test.Hspec (describe, hspec, it, shouldBe)

-- This provides the text for the test.
main :: IO ()
main = do
  hspec $ do
    describe "Main" $ do
      it "moodParse works" $ do
        let testText :: Text
            testText =
              [trimming|
                  {"mood": "Bad", "when": "2022-10-09 Mon 11:51"}
                  {"mood": "Good", "when": "2022-11-02 Sat 09:10"}|]
        parseMoodEntries (encodeUtf8 testText)
          `shouldBe` Right
            [ MoodEntry {moodWhen = fromJust $ parseTimeM False defaultTimeLocale "%Y-%m-%d %a %H:%M" "2022-10-09 Mon 11:51", moodWhat = Bad}
            , MoodEntry {moodWhen = fromJust $ parseTimeM False defaultTimeLocale "%Y-%m-%d %a %H:%M" "2022-11-02 Sat 09:10", moodWhat = Good}
            ]
