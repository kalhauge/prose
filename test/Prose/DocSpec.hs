{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Prose.DocSpec where

import SpecHelper

import Prose.Doc

-- base
import Data.List

-- text
import qualified Data.Text as Text

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

genSimpleInline :: (MonadGen m, MonadReader GenerateConfig m) => m SimpleInline
genSimpleInline = SimpleInline <$> genInline genSimpleInline

genInline :: (MonadGen m, MonadReader GenerateConfig m) => m i -> m (Inline i)
genInline genI = do 
  qoutes <- asks allowedQoutes
  Gen.recursive Gen.choice
    [ Gen.element [Comma, Colon, Hyphen]
    , do 
        c <- Gen.alpha
        word <- Gen.text (Range.linear 0 32) Gen.alphaNum
        pure $ Word (Text.cons c word)
    , do 
        n <- Gen.int (Range.linear 1 32)
        number <- Gen.text (Range.linear 1 n) Gen.digit
        pure $ Number number
    ]
    [ Qouted <$> genQoutedSentences qoute genI | qoute <- qoutes ]

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

genSimpleBlock :: (MonadGen m, MonadReader GenerateConfig m) => m SimpleBlock
genSimpleBlock = SimpleBlock <$> genBlock genSimpleBlock genSimpleInline

genBlock :: (MonadGen m, MonadReader GenerateConfig m) => m b -> m i -> m (Block b i)
genBlock mb mi = Gen.recursive Gen.choice
  [ Para <$> genSentences mi
  , Comment <$> Gen.list (Range.linear 1 3) (Gen.text (Range.linear 0 32) Gen.alphaNum)
  ]
  [ Items <$> Gen.nonEmpty (Range.linear 1 3) do
      em <- Gen.enumBounded
      blocks <- Gen.nonEmpty (Range.linear 1 3) mb
      return $ Item em Nothing blocks
  ]

genSimpleSection :: (MonadGen m, MonadReader GenerateConfig m) => m SimpleSection
genSimpleSection = SimpleSection <$> genSection genSimpleSection genSimpleBlock genSimpleInline

genSection :: (MonadGen m, MonadReader GenerateConfig m) => m s -> m b -> m i -> m (Section s b i)
genSection ms mb mi = do 
  sectionTitle <- genSentences mi
  sectionContent <- Gen.list (Range.linear 0 5) mb
  sectionSubs <- Gen.recursive Gen.choice 
    [ pure [] ] 
    [ Gen.list (Range.singleton 1) ms 
    , Gen.list (Range.singleton 2) ms 
    , Gen.list (Range.singleton 3) ms 
    ]
  return $ Section { .. }


