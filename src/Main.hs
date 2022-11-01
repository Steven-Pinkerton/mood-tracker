module Main where

import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Relude qualified
import Relude.List.Reexport ()
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (charset)

data Mood = Bad | Netural | Good | Great | Excellent
  deriving stock (Show)

data MoodEntry = MoodEntry
  { moodWhen :: UTCTime
  , moodWhat :: Mood
  }
  deriving stock (Show)

fileToByte :: MonadIO m => FilePath -> m ByteString
fileToByte = Relude.readFileBS

byteToText :: ByteString -> Text
byteToText = Relude.decodeUtf8

{-
filterDays :: Text -> Text
filterDays x =
  Relude.unlines $ Relude.filter (\w -> w `notElem` ["[", "]", "Mon", "Thu", "Wen", "Thr", "Fri", "Sat", "Sun"]) (Relude.lines x)
-}

filterMoods :: Text -> Text
filterMoods x =
  Relude.unlines $ Relude.filter (\w -> w `Prelude.elem` ["good", "neutral", "bad", "great", "excellent"]) (Relude.lines x)


moodChecker :: Text -> Mood
moodChecker x
  | x == "Good" = Good
  | x == "Nutral" = Netural
  | x == "Bad" = Bad
  | x == "Excellent" = Excellent
  | otherwise = error "Nothing has been provided"

moodList :: [Text] -> [Mood]
moodList = fmap moodChecker



timeFilter' :: Text -> Text
timeFilter' x =
  Relude.unwords $ Relude.filter (\w -> w `notElem` ["good", "netural", "bad", "great", "excellent", "[", "]", "Mon", "Tus", "Wen", "Thu", "Fri", "Sat", "Sun"]) (words x)


timeFilter :: Text -> [Text]
timeFilter x =
  Relude.filter (\w -> w  `notElem` ["good", "netural", "bad", "great", "excellent", "[", "]", "Mon", "Tus", "Wen", "Thu", "Fri", "Sat", "Sun"]) (words x)

timeList :: Text -> UTCTime
timeList x =
  let filteredList = toString $ Relude.unlines $ timeFilter x
    in parseTimeOrError True defaultTimeLocale "%F %H:%M" filteredList :: UTCTime

moodParse :: (UTCTime, Mood) -> MoodEntry
moodParse (x, y) = MoodEntry x y

moodParser :: Text -> [MoodEntry]
moodParser x =
  let targetList = lines x
      time = fmap timeList targetList
      moods = fmap filterMoods targetList
      mList = moodList moods
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
  targetFile <- fileToByte "src/moods.txt"
  let moodlist = moodParser $ byteToText targetFile
      response = renderHtml $ renderMoods moodlist
  respond $ responseLBS status200 [] response

main :: IO ()
main = do
  runApp 5000
