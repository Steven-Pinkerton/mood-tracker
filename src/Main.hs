{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Main where

import Data.Aeson (FromJSON, Object, ToJSON, decode, fromJSON, withObject, (.:))
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.ByteString.Lazy qualified as B
import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
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

data MoodRecord = MoodRecord
  { mood :: Mood
  , when :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- This will when applied to the file give us. MoodRecord {Good, 2022-10-09 Sun 11:51}.
-- FromJSON is in effect read but for JSON, as opposed to Read which is for strings.
-- The Alteratnive is to use decode. However
instance FromJSON MoodRecord where
  parseJSON = withObject "moodRecord" $ \o -> do
    mood <- o .: "mood"
    when <- o .: "when"
    return MoodRecord {..}

--
getJSON :: FilePath -> IO B.ByteString
getJSON = readFileLBS

--This gives up a MoodRecord  let MoodRecord = FromJSON x :: MoodRecord
moodParse' :: B.ByteString -> Maybe MoodRecord
moodParse' x =
  case decode x of
    Just record -> pure record
    Nothing -> Nothing

--This function applies

filterMoods :: Text -> Text
filterMoods x =
  unlines $ filter (\w -> w `elem` ["good", "neutral", "bad", "great", "excellent"]) (lines x)

parseMood :: Text -> Mood
parseMood x
  | x == "Good" = Good
  | x == "Nutral" = Netural
  | x == "Bad" = Bad
  | x == "Excellent" = Excellent
  | otherwise = error "Nothing has been provided"

timeFilter' :: Text -> Text
timeFilter' x =
  unwords $ filter (\w -> w `notElem` ["good", "netural", "bad", "great", "excellent", "[", "]", "Mon", "Tus", "Wen", "Thu", "Fri", "Sat", "Sun"]) (words x)

timeFilter :: Text -> [Text]
timeFilter x =
  filter (\w -> w `notElem` ["good", "netural", "bad", "great", "excellent", "[", "]", "Mon", "Tus", "Wen", "Thu", "Fri", "Sat", "Sun"]) (words x)

timeList :: Text -> UTCTime
timeList x =
  let filteredList = toString $ unlines $ timeFilter x
   in parseTimeOrError True defaultTimeLocale "%F %a %H:%M" filteredList :: UTCTime

--Delete this in favour of uncurry.
moodParse :: (UTCTime, Mood) -> MoodEntry
moodParse (x, y) = MoodEntry x y

parseMoodEntries :: Text -> [MoodEntry]
parseMoodEntries x =
  let inputList = lines x
      time = fmap timeList inputList
      moods = fmap filterMoods inputList
      mList = fmap parseMood moods
      ziped = zip time mList
   in fmap feederToParser ziped

feederToParser :: (UTCTime, Mood) -> MoodEntry
feederToParser x =
  let time = fst x
      mood = snd x
   in MoodEntry time mood

-- | This serves Html to an application.
renderMoods :: [MoodEntry] -> H.Html
renderMoods moods =
  H.docTypeHtml $ do
    H.body $ do
      H.h1 "Hello"
      H.meta H.! charset "UTF-8"
      H.ul $ do
        mapM_ H.li $ fmap renderMood moods

renderMood :: MoodEntry -> H.Html
renderMood = show

-- | This runs a web application, at the given port.
runApp :: Int -> IO ()
runApp port = do
  Prelude.putStrLn $ "Running HTTP server at http://127.0.0.1:" <> show port
  run port app

-- | This creates a web-server.
app :: Application
app _request respond = do
  targetFile <- readFileBS "src/moods.jsonl"
  let moodlist = parseMoodEntries $ decodeUtf8 targetFile
      response = renderHtml $ renderMoods moodlist
  respond $ responseLBS status200 [] response

main :: IO ()
main = do
  runApp 5000
