{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Prose.DocSpec where

-- base

import Data.Foldable
import Data.List
import Data.List.NonEmpty qualified as NE

-- text
import Data.Text qualified as Text

-- mtl
import Control.Monad.Reader

-- hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Prose.Builder
import Prose.Doc
import Prose.Recursion
import Prose.Simple
import SpecHelper

spec :: Spec
spec = do
  describe "countSentences" do
    let counter = fromSentences (cataA @Simple countSentences)
    it "should find one sentence in an open sentence" do
      counter
        ( OpenSentences
            (open' [mark' Comma])
        )
        `shouldBe` 1
    it "should find one sentence in two sentences" do
      counter
        ( ClosedSentences
            (closed' [mark' Comma] [Period])
            (Just (OpenSentences (open' [mark' Comma])))
        )
        `shouldBe` 2
    it "should find one sentence in two qouted sentences" do
      fromQoutedSentences
        (cataA @Simple countSentences)
        ( QoutedSentences
            Emph
            ( ClosedSentences
                (closed' [mark' Comma] [Period])
                (Just (OpenSentences (open' [mark' Comma])))
            )
        )
        `shouldBe` 2

data GenConfig = GenConfig
  { allowedQoutes :: [Qoute]
  , isFirstWord :: Bool
  }

genSimple :: MonadGen m => DocCoAlgebra m Simple
genSimple =
  natCoAlgebra
    ( `runReaderT`
        GenConfig [minBound .. maxBound] False
    )
    alg
 where
  (_, alg) = anaA embedRM generateR

genSimpleR :: MonadGen m => Generator m Simple
genSimpleR = fromCoAlgebra embedRM genSimple

generateR ::
  forall e m.
  (MonadGen m, MonadReader GenConfig m) =>
  Generator m e ->
  DocCoAlgebra m e
generateR Instance{..} = DocCoAlgebra{..}
 where
  toSection = do
    _sectionTitle <- toSentences
    _sectionContent <- Gen.list (Range.linear 0 3) onBlk
    _sectionSubs <-
      Gen.recursive
        Gen.choice
        [pure []]
        [ Gen.list (Range.singleton 1) onSec
        , Gen.list (Range.singleton 2) onSec
        , Gen.list (Range.singleton 3) onSec
        ]
    pure $ Section{..}

  toBlock =
    Gen.recursive
      Gen.choice
      [ Para
          <$> Gen.small toSentences
      , Comment
          <$> Gen.list
            (Range.linear 1 3)
            (Gen.text (Range.linear 0 4) Gen.alphaNum)
      , CodeBlock
          <$> Gen.maybe
            (Gen.element ["java", "markdown", "prose", "bibtex"])
          <*> Gen.list
            (Range.linear 0 4)
            (Gen.text (Range.linear 0 4) Gen.alphaNum)
      ]
      [ Items <$> Gen.nonEmpty (Range.linear 1 3) toItem
      , OrderedItems
          <$> Gen.enumBounded
          <*> Gen.nonEmpty (Range.linear 1 3) toOrderedItem
      ]

  toItem = do
    em <- Gen.enumBounded
    sens <- Gen.small toSentences
    blocks <- Gen.list (Range.linear 0 3) onBlk
    return $ Item em Nothing sens blocks

  toOrderedItem = do
    sens <- Gen.small toSentences
    blocks <- Gen.list (Range.linear 0 3) onBlk
    return $ OrderedItem Nothing sens blocks

  toInline = do
    qoutes <- asks allowedQoutes
    isFirst <- asks isFirstWord
    if isFirst
      then do
        c <- Gen.alpha
        word <-
          Gen.text (Range.linear 0 32) $
            Gen.choice
              [ Gen.alphaNum
              , Gen.element ['-', '\'', '$']
              ]
        pure . Word $ Text.cons c word
      else
        Gen.recursive
          Gen.choice
          [ Mark <$> Gen.enumBounded
          , pure Emdash
          , do
              c <- Gen.alpha
              word <-
                Gen.text (Range.linear 0 32) $
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
          [Qouted <$> toQoutedSentences' qoute | qoute <- qoutes]

  toQoutedSentences = do
    qoutes <- asks allowedQoutes
    Gen.choice [toQoutedSentences' qoute | qoute <- qoutes]

  toQoutedSentences' qoute = do
    sentences <- local (\a -> a{allowedQoutes = qoute `delete` allowedQoutes a}) toSentences
    pure $ QoutedSentences qoute sentences

  toSentences :: m (Sentences e)
  toSentences =
    Gen.recursive
      Gen.choice
      [ OpenSentences <$> onOpenSen
      , ClosedSentences <$> onClosedSen <*> pure Nothing
      ]
      [ Gen.subtermM toSentences \a -> do
          sens <- onClosedSen
          pure $ ClosedSentences sens (Just a)
      ]

  toClosedSentence :: m (Sentence 'Closed e)
  toClosedSentence =
    ClosedSentence
      <$> toSens
      <*> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded

  toOpenSentence :: m (Sentence 'Open e)
  toOpenSentence =
    OpenSentence <$> toSens

  toSens :: m (NE.NonEmpty (Inl e))
  toSens =
    (NE.:|)
      <$> local (\a -> a{isFirstWord = True}) onInl
      <*> Gen.list (Range.linear 0 10) onInl
