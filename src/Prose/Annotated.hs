{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Prose.Annotated where

import Control.Lens hiding (Simple)

import Prose.Doc
import Prose.Recursion
import Prose.Simple

data Ann a

data AnnSection a
  = AnnSection !a (Section (Ann a))
  deriving (Ord, Show, Eq)

data AnnBlock a
  = AnnBlock !a (Block (Ann a))
  deriving (Ord, Show, Eq)

data AnnInline a
  = AnnInline !a (Inline (Ann a))
  deriving (Ord, Show, Eq)

data AnnSentence (b :: SenType) a
  = AnnSentence !a (Sentence b (Ann a))
  deriving (Ord, Show, Eq)

instance DocR (Ann a) where
  type Sec (Ann a) = AnnSection a
  type Blk (Ann a) = AnnBlock a
  type Inl (Ann a) = AnnInline a
  type OpenSen (Ann a) = AnnSentence 'Open a
  type ClosedSen (Ann a) = AnnSentence 'Closed a

instance Ord a => OrdR (Ann a)
instance Show a => ShowR (Ann a)
instance Eq a => EqR (Ann a)

instance ProjectableR (Ann a) where
  projectR =
    DocMap $
      Instance
        { onSec = \(AnnSection _ a) -> a
        , onBlk = \(AnnBlock _ a) -> a
        , onInl = \(AnnInline _ a) -> a
        , onOpenSen = \(AnnSentence _ a) -> a
        , onClosedSen = \(AnnSentence _ a) -> a
        }

unAnnSec :: Lens' (AnnSection a) (Section (Ann a))
unAnnSec =
  lens (overSec projectR) (\(AnnSection a _) -> AnnSection a)

unAnnBlk :: Lens' (AnnBlock a) (Block (Ann a))
unAnnBlk =
  lens (overBlk projectR) (\(AnnBlock a _) -> AnnBlock a)

unAnnInl :: Lens' (AnnInline a) (Inline (Ann a))
unAnnInl =
  lens (overInl projectR) (\(AnnInline a _) -> AnnInline a)

unAnnSen :: Lens' (AnnSentence b a) (Sentence b (Ann a))
unAnnSen =
  lens
    (\(AnnSentence _ b) -> b)
    (\(AnnSentence a _) -> AnnSentence a)

instance LensR (Ann a) where
  lSec = unAnnSec
  lBlk = unAnnBlk
  lInl = unAnnInl
  lOpenSen = unAnnSen
  lClosedSen = unAnnSen

withConst :: a -> Unfix (Ann a) ~:> Ann a
withConst a =
  DocMap $
    Instance
      { onSec = AnnSection a
      , onBlk = AnnBlock a
      , onInl = AnnInline a
      , onOpenSen = AnnSentence a
      , onClosedSen = AnnSentence a
      }

toSimple :: Ann a ~:> Simple
toSimple = hylo projectR embedR

fromSimple :: a -> Simple ~:> Ann a
fromSimple a = hylo projectR (withConst a)
