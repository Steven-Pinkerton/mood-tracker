module MoodTracker.HTML where

import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (charset)

import MoodTracker.Parser (MoodEntry)

renderMoodEntry :: MoodEntry -> H.Html
renderMoodEntry moodentry = do
  H.span $ do
    H.toHtml (show moodentry :: String)

-- | This serves Html to an application.
renderMoods :: [MoodEntry] -> H.Html
renderMoods moods =
  H.docTypeHtml $ do
    H.body $ do
      H.h1 "Moods"
      H.meta H.! charset "UTF-8"
      H.ul $ do
        mapM_ H.li $ fmap renderMoodEntry moods

renderError :: Text -> H.Html
renderError error' = do
  H.span $ do
    H.toHtml (show error' :: String)

renderErrors :: String -> H.Html
renderErrors errors =
  H.docTypeHtml $ do
    H.body $ do
      H.h1 "Errors"
      H.meta H.! charset "UTF-8"
      H.ul $ do
        mapM_ H.li $ fmap renderError (lines (toText errors))
