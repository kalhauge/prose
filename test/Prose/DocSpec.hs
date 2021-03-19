{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Prose.Recursion
import Prose.Simple

spec :: Spec
spec = return ()

generate :: (EmbedableR e, MonadGen m) => GeneratorM e m
generate = generateR embedRM

generateR :: forall e m. 
  (MonadGen m, MonadReader [Qoute] m)
  => Unfix e ~:> Monadic e m
  -> GeneratorM e m 
generateR embed = GeneratorM {..}
 where
  DocMap 
    { overSec = oSec
    , overBlk = oBlk
    , overInl = oInl
    , overSen = oSen
    } = embed

  toSec :: m (Sec e)
  toSec = do
    sectionTitle <- toSentences
    sectionContent <- Gen.list (Range.linear 0 3) toBlk
    sectionSubs <- Gen.recursive Gen.choice
      [ pure [] ]
      [ Gen.list (Range.singleton 1) toSec 
      , Gen.list (Range.singleton 2) toSec
      , Gen.list (Range.singleton 3) toSec
      ]
    oSec $ Section { .. }

  toBlk = Gen.recursive Gen.choice
    [ Para 
      <$> Gen.small toSentences
    , Comment 
      <$> Gen.list (Range.linear 1 3) (Gen.text (Range.linear 0 4) Gen.alphaNum)
    ]
    [ Items 
      <$> Gen.nonEmpty (Range.linear 1 3) do
        em <- Gen.enumBounded
        sens <- Gen.small toSentences
        blocks <- Gen.list (Range.linear 0 3) toBlk
        return $ Item em Nothing sens blocks
    , OrderedItems 
      <$> Gen.enumBounded 
      <*> Gen.nonEmpty (Range.linear 1 3) do
        sens <- Gen.small toSentences
        blocks <- Gen.list (Range.linear 0 3) toBlk
        return $ OrderedItem Nothing sens blocks
    ]

  toInl = do
    qoutes <- ask
    oInl =<< Gen.recursive Gen.choice
      [ Mark <$> Gen.enumBounded
      , do
          c <- Gen.alpha
          word <- Gen.text (Range.linear 0 32) $
            Gen.choice
            [ Gen.alphaNum
            , Gen.element ['-', '\'', '$']
            ]
          pure . Word $ Text.cons c word
      , do
          c <- Gen.alpha
          word <- Gen.text (Range.linear 0 32) Gen.alphaNum
          x <- Gen.maybe (Gen.constant ".")
          pure $ Word (Text.cons c word <> fold x)
      , do
          word <- Gen.text (Range.linear 0 32) Gen.alphaNum
          pure $ Verbatim word
      , do
          n <- Gen.int (Range.linear 1 32)
          number <- Gen.text (Range.linear 1 n) Gen.digit
          pure $ Number number
      ]
      [ Qouted <$> toQoutedSentences qoute | qoute <- qoutes ]


  toQoutedSentences qoute = do
    sentences <- local (delete qoute) toSentences
    pure $ QoutedSentences qoute sentences

  toSentences = Gen.recursive Gen.choice
    [ OpenSentences <$> Gen.nonEmpty (Range.linear 1 3) toInl
    , ClosedSentences <$> toSen <*> pure Nothing
    ]
    [ Gen.subtermM toSentences \a -> do
        sens <- toSen 
        pure $ ClosedSentence sens (Just a)
    ]

  toSen :: m (MonadicSen m e)
  toSen = oSen =<< Gen.choice 
    [ ClosedSentence 
      <$> Gen.nonEmpty (Range.linear 1 10) toInl
    , OpenSentence
      <$> Gen.nonEmpty (Range.linear 1 10) toInl
      <*> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded
    ]



--newtype GenerateConfig = GenerateConfig
--  { allowedQoutes :: [Qoute]
--  }

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
-- 
-- 
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
