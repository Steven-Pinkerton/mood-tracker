{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson (FromJSON (parseJSON), ToJSON, decode, eitherDecode, withObject, (.:))
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import GHC.Generics ()
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Relude.List.Reexport ()
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (charset)

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
    when <- o .: "when"
    moodWhat <- o .: "mood"
    moodWhen <- parseTimeM False defaultTimeLocale "%Y-%m-%d %a %H:%M" when
    return MoodEntry {moodWhen, moodWhat}

--Consider Traversal, build it with regards to moodPrase
moodParse :: B.ByteString -> Either String [MoodEntry]
moodParse x =  traverse eitherDecode (BL.lines x)


--This function creates a list of [MoodEntry] from a MoodEntry.
listMoods' :: MoodEntry -> [MoodEntry]
listMoods' x = [MoodEntry (moodWhen x) (moodWhat x)]

-- | This serves Html to an application.
renderMoods' :: Either String [MoodEntry] -> H.Html
renderMoods' moods = 
  H.docTypeHtml $ do
    H.body $ do
      H.h1 "Moods"
      H.meta H.! charset "UTF-8"
      H.ul $ do
        mapM_ H.li $ fmap show moods

-- | This runs a web application, at the given port.
runApp :: Int -> IO ()
runApp port = do
  Prelude.putStrLn $ "Running HTTP server at http://127.0.0.1:" <> show port
  run port app'

app' :: Application
app' _request respond = do
  targetFile <- readFileLBS "src/moods.jsonl"
  let listOfMoods = moodParse targetFile
      response = renderHtml $ renderMoods' listOfMoods
   in respond $ responseLBS status200 [] response

main :: IO ()
main = do
  targetFile <- readFileLBS "src/moods.jsonl"
  let listOfMoods = moodParse targetFile
   in case listOfMoods of
        Right success -> runApp 7000
        Left err -> print err
