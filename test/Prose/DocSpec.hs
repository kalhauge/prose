module Prose.DocSpec where

import SpecHelper

import Prose.Doc

-- hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


spec :: Spec
spec = return ()

genInline :: MonadGen m => m Inline
genInline = Gen.choice
  [ Gen.element [Comma, Colon, Hyphen]
  , do 
      word <- Gen.text (Range.linear 1 32) Gen.alphaNum
      pure $ Word word
  ]

genSentence :: MonadGen m => m i -> m (Sentence i)
genSentence genI = Sentence 
  <$> Gen.list (Range.linear 1 30) genI 
  <*> Gen.list (Range.linear 0 4) (Gen.element [Question, Period, Exclamation])
