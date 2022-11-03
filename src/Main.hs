{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import GHC.Generics ()
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Relude.List.Reexport ()
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (charset)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types ()

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
   in parseTimeOrError True defaultTimeLocale "%F %H:%M" filteredList :: UTCTime

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
  targetFile <- readFileBS "src/moods.txt"
  let moodlist = parseMoodEntries $ decodeUtf8 targetFile
      response = renderHtml $ renderMoods moodlist
  respond $ responseLBS status200 [] response

main :: IO ()
main = do
  runApp 5000
