module Main where

import MoodTracker.HTML (renderErrors, renderMoods)
import MoodTracker.Parser (parseMoodEntries)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

app' :: Application
app' _request respond = do
  fileContents <- readFileLBS "src/moods.jsonl"
  case parseMoodEntries fileContents of
    Right success -> respond $ responseLBS status200 [] (renderHtml $ renderMoods success)
    Left errors -> respond $ responseLBS status200 [] (renderHtml $ renderErrors errors)

main :: IO ()
main = do
  putStrLn "Running HTTP server at http://127.0.0.1:3000"
  run 3000 app'
