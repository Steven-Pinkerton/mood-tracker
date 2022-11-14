module MoodTracker.HTML where

import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (charset)

import MoodTracker.Parser (MoodEntry)

renderMood :: MoodEntry -> H.Html
renderMood x = do
  H.span $ do
    H.toHtml (show x :: String)

-- | This serves Html to an application.
renderMoods' :: Either String [MoodEntry] -> H.Html
renderMoods' moods =
  case moods of
    Right suc ->
      H.docTypeHtml $ do
        H.body $ do
          H.h1 "Moods"
          H.meta H.! charset "UTF-8"
          H.ul $ do
            mapM_ H.li $ fmap renderMood suc
    Left err -> H.docTypeHtml $ do
      H.body $ do
        H.h1 "Moods"
        H.meta H.! charset "UTF-8"
        H.ul $ do
          mapM_ H.li $ fmap H.toHtml err
