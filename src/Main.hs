{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy qualified as B
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import GHC.Generics ()
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Relude.List.Reexport ()
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (charset)

data Mood = Bad | Netural | Good | Great | Excellent
  deriving stock (Show, Eq, Generic)

instance FromJSON Mood
instance ToJSON Mood

data MoodEntry = MoodEntry
  { moodWhen :: UTCTime
  , moodWhat :: Mood
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MoodEntry
instance ToJSON MoodEntry

--This reads a JSON file
getJSON :: FilePath -> IO B.ByteString
getJSON = readFileLBS

--This gives up a MoodRecord when provided with a lazy bytestring of the correct format.
moodParse' :: B.ByteString -> Maybe MoodEntry
moodParse' x = decode x :: Maybe MoodEntry

--This tests if the Maybe MoodRecord supplied contains a Just value or Nothing.
testForNothing :: Maybe MoodEntry -> Bool
testForNothing x =
  case x of
    Just _ -> True
    Nothing -> False

fromResult :: Maybe MoodEntry -> MoodEntry
fromResult = fromJust

--This function applies moodParse' to get a MoodEntry
applyParse' :: MoodEntry -> [MoodEntry]
applyParse' x = [MoodEntry (moodWhen x) (moodWhat x)]

-- | This serves Html to an application.
renderMoods' :: [MoodEntry] -> H.Html
renderMoods' moods =
  H.docTypeHtml $ do
    H.body $ do
      H.h1 "Hello"
      H.meta H.! charset "UTF-8"
      H.ul $ do
        mapM_ H.li $ fmap renderMood moods

app' :: Application
app' _request respond = do
  targetFile <- getJSON "src/oneMood.jsonl"
  let moodlist = moodParse' targetFile
      response = renderHtml $ renderMoods' $ applyParse' $ fromResult moodlist
   in respond $ responseLBS status200 [] response

renderMood :: MoodEntry -> H.Html
renderMood = show

main :: IO ()
main = do
  targetFile <- getJSON "src/oneMood.jsonl"
  let moodList = moodParse' targetFile
   in if testForNothing moodList
        then run 5000 app'
        else putStrLn "The file supplied is malformed"
