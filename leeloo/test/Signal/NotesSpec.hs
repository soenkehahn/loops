module Signal.NotesSpec where

import Test.Hspec
import Signal.Notes
import Test.Utils

spec :: Spec
spec = do
  describe "pitch" $ do
    it "pitches 2 ^ (1 / 12) for one semitone" $ do
      pitch 1 100 `shouldBeCloseTo` (100 * 2 ** (1 / 12))

    it "pitches multiple semitones" $ do
      pitch 2 100 `shouldBeCloseTo` (100 * 2 ** (2 / 12))

    it "pitches one octave for 12 semitones" $ do
      pitch 12 100 `shouldBeCloseTo` 200

    it "can pitch down" $ do
      pitch (-12) 100 `shouldBeCloseTo` 50

    it "a''' comes out as 440 Hz" $ do
      a''' `shouldBeCloseTo` 440

    it "works for micro pitches" $ do
      pitch (- 0.5) a''' `shouldBeCloseTo` (440 * 2 ** (- 0.5 / 12))
