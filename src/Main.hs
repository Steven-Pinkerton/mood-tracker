{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson (FromJSON (parseJSON), ToJSON, decode, eitherDecode, withObject, (.:))
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
  deriving stock (Show, Eq, Read, Generic)

instance FromJSON Mood
instance ToJSON Mood

data MoodEntry = MoodEntry
  { moodWhen :: UTCTime
  , moodWhat :: Mood
  }
  deriving stock (Show, Eq, Generic)
instance ToJSON MoodEntry

instance FromJSON MoodEntry where
  parseJSON = withObject "moodRecord" $ \o -> do
    moodWhen <- o .: "when"
    moodWhat <- o .: "mood"
    return MoodEntry {..}

--This reads a JSON file
getJSON :: FilePath -> IO B.ByteString
getJSON = readFileLBS

--This gives up a MoodRecord when provided with a lazy bytestring of the correct format.
moodParse' :: B.ByteString -> Either String MoodEntry
moodParse' = eitherDecode

--This tests if the Maybe MoodRecord supplied contains a Just value or Nothing.
testForNothing :: Maybe MoodEntry -> Bool
testForNothing x =
  case x of
    Just _ -> True
    Nothing -> False

--This removes the value from Maybe
--Due to tests in main catching any malformed data prior to being introduced to the function.
{- fromResult :: MoodEntry -> MoodEntry
fromResult = fromJust
-}

--This function creates a list of [MoodEntry] from a MoodEntry.
listMoods' :: MoodEntry -> [MoodEntry]
listMoods' x = [MoodEntry (moodWhen x) (moodWhat x)]

-- | This serves Html to an application.
renderMoods' :: [MoodEntry] -> H.Html
renderMoods' moods =
  H.docTypeHtml $ do
    H.body $ do
      H.h1 "Hello"
      H.meta H.! charset "UTF-8"
      H.ul $ do
        mapM_ H.li $ fmap renderMood moods

renderMood :: MoodEntry -> H.Html
renderMood = show

-- | This runs a web application, at the given port.

{- runApp :: Int -> IO ()
runApp port = do
  Prelude.putStrLn $ "Running HTTP server at http://127.0.0.1:" <> show port
  run port app'

app' :: Application
app' _request respond = do
  targetFile <- getJSON "src/oneMood.jsonl"
  let moodlist = moodParse' targetFile
      response = renderHtml $ renderMoods' $ listMoods' moodlist
   in respond $ responseLBS status200 [] response
-}

main :: IO ()
main = do
  targetFile <- getJSON "src/oneMood.jsonl"
  let moodList = moodParse' targetFile
   in case moodList of
        Right success -> print success
        Left err -> print err
