{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Prose.DocSpec where

-- base
import Data.List
import Data.Foldable

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

spec :: Spec
spec = return ()

generate :: (EmbedableR e, MonadGen m) => Generator m e
generate = 
  mapDoc (runReaderTR [minBound..maxBound]) 
  $ anaA embedRM generateR

generateR :: forall e m. 
  (MonadGen m, MonadReader [Qoute] m)
  => Generator m e
  -> DocCoAlgebra m e
generateR Instance {..} = DocCoAlgebra {..}
 where
  toSection = do
    sectionTitle <- toSentences
    sectionContent <- Gen.list (Range.linear 0 3) onBlk 
    sectionSubs <- Gen.recursive Gen.choice
      [ pure [] ]
      [ Gen.list (Range.singleton 1) onSec 
      , Gen.list (Range.singleton 2) onSec
      , Gen.list (Range.singleton 3) onSec
      ]
    pure $ Section { .. }

  toBlock = Gen.recursive Gen.choice
    [ Para 
      <$> Gen.small toSentences
    , Comment 
      <$> Gen.list 
        (Range.linear 1 3) 
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
    qoutes <- ask
    Gen.recursive Gen.choice
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
      [ Qouted <$> toQoutedSentences' qoute | qoute <- qoutes ]

  toQoutedSentences = do
    qoutes <- ask
    Gen.choice [ toQoutedSentences' qoute | qoute <- qoutes ]

  toQoutedSentences' qoute = do
    sentences <- local (delete qoute) toSentences
    pure $ QoutedSentences qoute sentences

  toSentences :: m (Sentences e)
  toSentences = Gen.recursive Gen.choice
    [ OpenSentences <$> onOpenSen
    , ClosedSentences <$> onClosedSen <*> pure Nothing
    ]
    [ Gen.subtermM toSentences \a -> do
        sens <- onClosedSen
        pure $ ClosedSentences sens (Just a)
    ]

  toClosedSentence = ClosedSentence
    <$> Gen.nonEmpty (Range.linear 1 10) onInl
    <*> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded

  toOpenSentence = OpenSentence 
    <$> Gen.nonEmpty (Range.linear 1 10) onInl
