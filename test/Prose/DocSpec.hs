{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Prose.DocSpec where

-- base
import Data.List
import Data.Foldable
import qualified Data.List.NonEmpty as NE

-- text
import qualified Data.Text as Text

-- mtl
import Control.Monad.Reader

-- hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import SpecHelper
import Prose.Doc


spec :: Spec
spec = return ()


data Monadic e (m :: * -> *)

newtype MonadicSen e (m :: * -> *) (b :: SenType) = MonadicSen 
  { unMonadicSen :: m (Sen e b)
  }

instance DocR (Monadic e m) where
  type Sec (Monadic e m) = m (Sec e) 
  type Blk (Monadic e m) = m (Blk e) 
  type Inl (Monadic e m) = m (Inl e) 
  type Sen (Monadic e m) = MonadicSen e m


generate :: forall a m. a ~ Value [Qoute] => a ~:> Monadic (Unfix a) m
generate = DocMap {..}
 where
  overSec :: [Qoute] -> m (Section a)
  overSec qoutes = do
    sectionTitle <- overSentences []
    sectionContent <- Gen.list (Range.linear 0 3) []
    sectionSubs <- Gen.recursive Gen.choice
      [ pure [] ]
      [ Gen.list (Range.singleton 1) [] 
      , Gen.list (Range.singleton 2) []
      , Gen.list (Range.singleton 3) []
      ]
    return $ Section { .. }

  -- overSentences qoutes = Gen.recursive Gen.choice
  --   [ OpenSentences <$> Gen.nonEmpty (Range.linear 1 3) genI
  --   , ClosedSentences <$> genSentence genI <*> pure Nothing
  --   ]
  --   [ Gen.subtermM (overSentences qoutes) \a -> do
  --       sens <- genSentence genI
  --       pure $ ClosedSentence sens (Just a)
  --   ]

-- newtype GenerateConfig = GenerateConfig
--   { allowedQoutes :: [Qoute]
--   }
-- 
-- genConfig :: GenerateConfig
-- genConfig = GenerateConfig
--   { allowedQoutes = [minBound .. maxBound]
--   }
-- 
-- -- generate :: ReaderT GenerateConfig m a -> m a
-- -- generate ma = runReaderT ma genConfig
-- 
-- genSimpleInline ::
--   (MonadGen m, MonadReader GenerateConfig m)
--   => m Inline'
-- genSimpleInline =
--   Inline' <$> genInline genSimpleInline
-- 
-- genInline :: (MonadGen m, MonadReader GenerateConfig m) => m i -> m (Inline i)
-- genInline genI = do
--   qoutes <- asks allowedQoutes
--   Gen.recursive Gen.choice
--     [ Gen.element [Comma, Colon]
--     , do
--         c <- Gen.alpha
--         word <- Gen.text (Range.linear 0 32) $
--           Gen.choice
--           [ Gen.alphaNum
--           , Gen.element ['-', '\'', '$']
--           ]
--         pure . Word $ Text.cons c word
--     , do
--         c <- Gen.alpha
--         word <- Gen.text (Range.linear 0 32) Gen.alphaNum
--         x <- Gen.maybe (Gen.constant ".")
--         pure $ Word (Text.cons c word <> fold x)
--     , do
--         word <- Gen.text (Range.linear 0 32) Gen.alphaNum
--         pure $ Verbatim word
--     , do
--         n <- Gen.int (Range.linear 1 32)
--         number <- Gen.text (Range.linear 1 n) Gen.digit
--         pure $ Number number
--     ]
--     [ Qouted <$> genQoutedSentences qoute genI | qoute <- qoutes ]
-- 
-- genQoutedSentences :: (MonadGen m, MonadReader GenerateConfig m) => Qoute -> m i -> m (QoutedSentences i)
-- genQoutedSentences qoute genI = do
--   sentences <- local (\a -> a { allowedQoutes =  delete qoute (allowedQoutes a)})
--     (genSentences genI)
--   pure $ QoutedSentences qoute sentences
-- 
-- genSentence :: (MonadGen m, MonadReader GenerateConfig m) => m i -> m (Sentence i)
-- genSentence genI = Sentence
--   <$> Gen.nonEmpty (Range.linear 1 10) genI
--   <*> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded
-- 
-- genSentences :: (MonadGen m, MonadReader GenerateConfig m) => m i -> m (Sentences i)
-- genSentences genI = Gen.recursive Gen.choice
--   [ OpenSentence <$> Gen.nonEmpty (Range.linear 1 3) genI
--   , ClosedSentence <$> genSentence genI <*> pure Nothing
--   ]
--   [ Gen.subtermM (genSentences genI) \a -> do
--       sens <- genSentence genI
--       pure $ ClosedSentence sens (Just a)
--   ]
-- 
-- genSimpleBlock :: (MonadGen m, MonadReader GenerateConfig m) => m Block'
-- genSimpleBlock = Block' <$> genBlock genSimpleBlock genSimpleInline
-- 
-- genBlock :: (MonadGen m, MonadReader GenerateConfig m) => m b -> m i -> m (Block b i)
-- genBlock mb mi = Gen.recursive Gen.choice
--   [ Para <$> Gen.small (genSentences mi)
--   , Comment <$> Gen.list (Range.linear 1 3) (Gen.text (Range.linear 0 4) Gen.alphaNum)
--   ]
--   [ Items <$> Gen.nonEmpty (Range.linear 1 3) do
--       em <- Gen.enumBounded
--       sens <- Gen.small(genSentences mi)
--       blocks <- Gen.list (Range.linear 0 3) mb
--       return $ Item em Nothing sens blocks
--   , OrderedItems <$> Gen.enumBounded <*> Gen.nonEmpty (Range.linear 1 3) do
--       sens <- Gen.small (genSentences mi)
--       blocks <- Gen.list (Range.linear 0 3) mb
--       return $ OrderedItem Nothing sens blocks
--   ]
-- 
-- genSingeWordSentence ::
--   (MonadGen m, MonadReader GenerateConfig m)
--   => m i
--   -> m (Sentence i)
-- genSingeWordSentence genI = Sentence
--   <$> ((NE.:| []) <$> genI)
--   <*> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded
-- 
-- genSingleWordSentences ::
--   (MonadGen m, MonadReader GenerateConfig m)
--   => m i
--   -> m (Sentences i)
-- genSingleWordSentences genI = Gen.recursive Gen.choice
--   [ OpenSentence . (NE.:| []) <$> genI
--   , ClosedSentence <$> genSingeWordSentence genI <*> pure Nothing
--   ]
--   [ Gen.subtermM (genSingleWordSentences genI) \a -> do
--       sens <- genSingeWordSentence genI
--       pure $ ClosedSentence sens (Just a)
--   ]
-- 
-- genItemsBlock :: (MonadGen m, MonadReader GenerateConfig m) => m Block'
-- genItemsBlock = do
--   Block' . Items <$> Gen.nonEmpty (Range.linear 1 3) do
--     em <- Gen.enumBounded
--     sens <- Gen.small $ genSingleWordSentences genWord
--     blocks <- Gen.list (Range.linear 0 3) (Block' <$> genBlock genItemsBlock genWord)
--     pure $ Item em Nothing sens blocks
--  where
--    genWord = Inline' . Word <$> Gen.text (Range.singleton 1) Gen.alpha
-- 
-- genSimpleSection ::
--   (MonadGen m, MonadReader GenerateConfig m)
--   => m Section'
-- genSimpleSection =
--   Section' <$> genSection genSimpleSection genSimpleBlock genSimpleInline
-- 
-- genSection :: (MonadGen m, MonadReader GenerateConfig m) => m s -> m b -> m i -> m (Section s b i)
-- genSection ms mb mi = do
--   sectionTitle <- genSentences mi
--   sectionContent <- Gen.list (Range.linear 0 5) mb
--   sectionSubs <- Gen.recursive Gen.choice
--     [ pure [] ]
--     [ Gen.list (Range.singleton 1) ms
--     , Gen.list (Range.singleton 2) ms
--     , Gen.list (Range.singleton 3) ms
--     ]
--   return $ Section { .. }
-- 
-- 
