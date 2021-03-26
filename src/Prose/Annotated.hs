{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Prose.Annotated where

import Prose.Doc
import Prose.Simple
import Prose.Recursion

data Ann a

data AnnSection a = 
  AnnSection !a (Section (Ann a))
  deriving (Ord, Show, Eq)

data AnnBlock a = 
  AnnBlock !a (Block (Ann a))
  deriving (Ord, Show, Eq)

data AnnInline a = 
  AnnInline !a (Inline (Ann a))
  deriving (Ord, Show, Eq)

data AnnSentence (b :: SenType) a = 
  AnnSentence !a (Sentence b (Ann a))
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
  projectR = DocMap $ Instance 
    { onSec = \(AnnSection _ a) -> a
    , onBlk = \(AnnBlock _ a) -> a
    , onInl = \(AnnInline _ a) -> a
    , onOpenSen = \(AnnSentence _ a) -> a
    , onClosedSen = \(AnnSentence _ a) -> a
    }

withConst :: a -> Unfix (Ann a) ~:> Ann a
withConst a = DocMap $ Instance 
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

