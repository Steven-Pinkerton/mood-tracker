module MoodTracker.HTML where

import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

import MoodTracker.Parser (MoodEntry)

-- | This serves Html to an application.
renderMoodEntries :: [MoodEntry] -> H.Html
renderMoodEntries moods = do
  H.h1 "Moods"
  H.ul $ do
    forM_ moods $ \mood -> do
      H.li $ renderMoodEntry mood
  where
    renderMoodEntry :: MoodEntry -> H.Html
    renderMoodEntry moodentry = do
      H.span $ do
        H.toHtml (show moodentry :: String)

-- | This serves errors that have arisen when trying to read a moodEntry as HTML.
renderErrors :: String -> H.Html
renderErrors errors = do
  H.h1 "Errors"
  H.ul $ do
    forM_ (lines $ toText errors) $ \err -> do
      H.li $ renderError err
  where
    -- This takes an error and returns it as HTML.
    renderError :: Text -> H.Html
    renderError error' = do
      H.span $ do
        H.toHtml (show error' :: String)

renderData :: Either String [MoodEntry] -> H.Html
renderData dat =
  H.docTypeHtml $ do
    H.head $ do
      H.meta H.! A.charset "UTF-8"
    H.body $ do
      case dat of
        Left err -> renderErrors err
        Right v -> renderMoodEntries v
