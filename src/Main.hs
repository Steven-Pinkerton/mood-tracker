module Main where

import MoodTracker.HTML (renderMoods, renderErrors)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import MoodTracker.Parser ( moodParse )

app' :: Application
app' _request respond = do
  result <- readFileLBS "src/moods.jsonl"
  case moodParse result of
    Right success -> respond $ responseLBS status200 [] (renderHtml $ renderMoods success)
    Left errors   -> respond $ responseLBS status200 [] (renderHtml $ renderErrors errors)

main :: IO ()
main = do
  putStrLn "Running HTTP server at http://127.0.0.1:9000"
  run 9000 app'