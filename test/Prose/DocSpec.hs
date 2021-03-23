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

generate :: (EmbedableR e, MonadGen m) => InstanceM e m
generate = mapDoc 
  (changeRM (`runReaderT` [minBound .. maxBound])) 
  (generateR embedRM)

generateR :: forall e m. 
  (MonadGen m, MonadReader [Qoute] m)
  => Unfix e ~:> Monadic e m
  -> Instance (Monadic e m)
generateR embed = Instance {..}
 where
  DocMap 
    { overSec = oSec
    , overBlk = oBlk
    , overInl = oInl
    , overSen = oSen 
    } = embed

  getSec :: m (Sec e)
  getSec = do
    sectionTitle <- getSentences
    sectionContent <- Gen.list (Range.linear 0 3) getBlk
    sectionSubs <- Gen.recursive Gen.choice
      [ pure [] ]
      [ Gen.list (Range.singleton 1) getSec 
      , Gen.list (Range.singleton 2) getSec
      , Gen.list (Range.singleton 3) getSec
      ]
    oSec $ Section { .. }

  getBlk = oBlk =<< Gen.recursive Gen.choice
    [ Para 
      <$> Gen.small getSentences
    , Comment 
      <$> Gen.list (Range.linear 1 3) (Gen.text (Range.linear 0 4) Gen.alphaNum)
    ]
    [ Items 
      <$> Gen.nonEmpty (Range.linear 1 3) do
        em <- Gen.enumBounded
        sens <- Gen.small getSentences
        blocks <- Gen.list (Range.linear 0 3) getBlk
        return $ Item em Nothing sens blocks
    , OrderedItems 
      <$> Gen.enumBounded 
      <*> Gen.nonEmpty (Range.linear 1 3) do
        sens <- Gen.small getSentences
        blocks <- Gen.list (Range.linear 0 3) getBlk
        return $ OrderedItem Nothing sens blocks
    ]

  getInl = do
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
      [ Qouted <$> getQoutedSentences qoute | qoute <- qoutes ]


  getQoutedSentences qoute = do
    sentences <- local (delete qoute) getSentences
    pure $ QoutedSentences qoute sentences

  getSentences :: m (Sentences e)
  getSentences = Gen.recursive Gen.choice
    [ OpenSentences <$> unMonadicSen getOpenSen
    , ClosedSentences <$> unMonadicSen getClosedSen <*> pure Nothing
    ]
    [ Gen.subtermM getSentences \a -> do
        sens <- unMonadicSen getClosedSen
        pure $ ClosedSentences sens (Just a)
    ]

  getClosedSen :: MonadicSen e m 'Closed
  getClosedSen = MonadicSen $ do 
    sen <- ClosedSentence
      <$> Gen.nonEmpty (Range.linear 1 10) getInl
      <*> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded
    unMonadicSen $ oSen sen

  getOpenSen :: MonadicSen e m 'Open
  getOpenSen = MonadicSen $ do 
    sen <- OpenSentence <$> Gen.nonEmpty (Range.linear 1 10) getInl
    unMonadicSen $ oSen sen


