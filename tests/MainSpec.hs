module MainSpec where

import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  describe "Main" $ do
    it "renderMood works" $ do
      renderMood (MoodEntry {moodWhen = 2022 - 10 - 09 11 : 51 : 00 UTC, moodWhat = Good}) `shouldBe` "<span>MoodEntry {moodWhen = 2022-10-09 11:51:00 UTC, moodWhat = Good}</span>"

    it "renderMoods' works" $ do
      renderMoods' (Right [MoodEntry {moodWhen = 2022 - 10 - 09 11 : 51 : 00 UTC, moodWhat = Good}]) `shouldBe` "<html><body><head>Moods</head><ul><li><span>MoodEntry {moodWhen = 2022-10-09 11:51:00 UTC, moodWhat = Good}</span></li></ul></body></html>"
