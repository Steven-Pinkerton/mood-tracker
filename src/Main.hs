module Main where

import MoodTracker.HTML (renderData)
import MoodTracker.Parser (parseMoodEntries)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

app' :: Application
app' _request respond = do
  fileContents <- readFileLBS "src/moods.jsonl"
  respondHtml . renderHtml . renderData $
    parseMoodEntries fileContents
  where
    respondHtml = respond . responseLBS status200 []

main :: IO ()
main = do
  putStrLn "Running HTTP server at http://127.0.0.1:3000"
  run 3000 app'
