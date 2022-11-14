module Main where

import MoodTracker.HTML (renderMoods')
import MoodTracker.Parser (moodParse)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

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
        Right _ -> runApp 8000
        Left err -> print err
