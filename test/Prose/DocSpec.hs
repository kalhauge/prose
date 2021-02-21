{-# LANGUAGE FlexibleContexts #-}
module Prose.DocSpec where

import SpecHelper

import Prose.Doc

-- base
import Data.List

-- mtl 
import Control.Monad.Reader

-- hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


spec :: Spec
spec = return ()

newtype GenerateConfig = GenerateConfig 
  { allowedQoutes :: [Qoute]
  }

genConfig :: GenerateConfig
genConfig = GenerateConfig
  { allowedQoutes = [minBound .. maxBound]
  }

genInline :: (MonadGen m, MonadReader GenerateConfig m) => m i -> m (Inline i)
genInline genI = do 
  qoutes <- asks allowedQoutes
  Gen.recursive Gen.choice
    [ Gen.element [Comma, Colon, Hyphen]
    , do 
        word <- Gen.text (Range.linear 1 32) Gen.alphaNum
        pure $ Word word
    ]
    [ Qouted <$> genQoutedSentences qoute genI 
    | qoute <- qoutes
    ]

genQoutedSentences :: (MonadGen m, MonadReader GenerateConfig m) => Qoute -> m i -> m (QoutedSentences i)
genQoutedSentences qoute genI = do 
  sentences <- local (\a -> a { allowedQoutes =  delete qoute (allowedQoutes a)}) 
    (genSentences genI)
  pure $ QoutedSentences qoute sentences

genSentence :: (MonadGen m, MonadReader GenerateConfig m) => m i -> m (Sentence i)
genSentence genI = Sentence 
  <$> Gen.nonEmpty (Range.linear 1 30) genI 
  <*> Gen.nonEmpty (Range.linear 1 4) Gen.enumBounded

genSentences :: (MonadGen m, MonadReader GenerateConfig m) => m i -> m (Sentences i)
genSentences genI = do 
  xs <- Gen.list (Range.linear 0 10) (genSentence genI)
  ys <- Gen.list (Range.linear (if null xs then 1 else 0) 10) genI 
  return $ Sentences xs ys

genSimpleInline :: (MonadGen m, MonadReader GenerateConfig m) => m SimpleInline
genSimpleInline = SimpleInline <$> genInline genSimpleInline
