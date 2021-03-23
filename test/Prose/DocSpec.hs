{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

generate :: (EmbedableR e, MonadGen m) => Monadic m e
generate = mapDoc 
  (runReaderTR [minBound .. maxBound])
  (generateR embedRM)

generateR :: forall e m. 
  (MonadGen m, MonadReader [Qoute] m)
  => Unfix e ~:> Apply m e
  -> Monadic m e
generateR embed = Instance {..}
 where
  onSec :: m (Sec e)
  onSec = do
    sectionTitle <- onSentences
    sectionContent <- Gen.list (Range.linear 0 3) onBlk
    sectionSubs <- Gen.recursive Gen.choice
      [ pure [] ]
      [ Gen.list (Range.singleton 1) onSec 
      , Gen.list (Range.singleton 2) onSec
      , Gen.list (Range.singleton 3) onSec
      ]
    overSec embed $ Section { .. }

  onBlk = overBlk embed =<< Gen.recursive Gen.choice
    [ Para 
      <$> Gen.small onSentences
    , Comment 
      <$> Gen.list (Range.linear 1 3) (Gen.text (Range.linear 0 4) Gen.alphaNum)
    ]
    [ Items 
      <$> Gen.nonEmpty (Range.linear 1 3) do
        em <- Gen.enumBounded
        sens <- Gen.small onSentences
        blocks <- Gen.list (Range.linear 0 3) onBlk
        return $ Item em Nothing sens blocks
    , OrderedItems 
      <$> Gen.enumBounded 
      <*> Gen.nonEmpty (Range.linear 1 3) do
        sens <- Gen.small onSentences
        blocks <- Gen.list (Range.linear 0 3) onBlk
        return $ OrderedItem Nothing sens blocks
    ]

  onInl = do
    qoutes <- ask
    overInl embed =<< Gen.recursive Gen.choice
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
      [ Qouted <$> onQoutedSentences qoute | qoute <- qoutes ]


  onQoutedSentences qoute = do
    sentences <- local (delete qoute) onSentences
    pure $ QoutedSentences qoute sentences

  onSentences :: m (Sentences e)
  onSentences = Gen.recursive Gen.choice
    [ OpenSentences <$> onOpenSen
    , ClosedSentences <$> onClosedSen <*> pure Nothing
    ]
    [ Gen.subtermM onSentences \a -> do
        sens <- onClosedSen
        pure $ ClosedSentences sens (Just a)
    ]

  onClosedSen = do 
    sen <- ClosedSentence
      <$> Gen.nonEmpty (Range.linear 1 10) onInl
      <*> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded
    overClosedSen embed sen

  onOpenSen = do
    sen <- OpenSentence <$> Gen.nonEmpty (Range.linear 1 10) onInl
    overOpenSen embed sen


