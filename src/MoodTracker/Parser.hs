{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module MoodTracker.Parser where

import Data.Aeson (FromJSON (parseJSON), ToJSON, eitherDecode, withObject, (.:))
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import GHC.Generics ()
import Relude.List.Reexport ()

data Mood = Bad | Neutral | Good | Great | Excellent
  deriving stock (Show, Eq, Read, Generic)

instance FromJSON Mood
instance ToJSON Mood

data MoodEntry = MoodEntry
  { moodWhen :: UTCTime
  , moodWhat :: Mood
  }
  deriving stock (Show, Eq, Generic)
instance ToJSON MoodEntry

--We can alter this to do more then just instantly derive a UTCTime. Alternatively we can alter the format of the file itself.
instance FromJSON MoodEntry where
  parseJSON = withObject "moodRecord" $ \o -> do
    when' <- o .: "when"
    moodWhat <- o .: "mood"
    moodWhen <- parseTimeM False defaultTimeLocale "%Y-%m-%d %a %H:%M" when'
    return MoodEntry {moodWhen, moodWhat}

--Consider Traversal, build it with regards to moodPrase
moodParse :: B.ByteString -> Either String [MoodEntry]
moodParse x = traverse eitherDecode (BL.lines x)
