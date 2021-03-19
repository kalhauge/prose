{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Prose.Recursion where

-- mtl
import Control.Monad.Reader

-- base
import Unsafe.Coerce
import Control.Monad
import Data.Void
import Control.Category
import Data.Monoid
import Data.Foldable
import Prelude hiding (Word, (.), id)

import Prose.Doc

-- | DocMaps is a function that can be run on a Document structure
data Instance e = Instance 
  { getSec :: Sec e
  , getBlk :: Blk e
  , getInl :: Inl e
  , getSen :: forall b. Sen e b
  }

-- | DocMaps is a function that can be run on a Document structure
data e ~:> e' = DocMap
  { overSec :: Sec e -> Sec e'
  , overBlk :: Blk e -> Blk e'
  , overInl :: Inl e -> Inl e'
  , overSen :: forall b. Sen e b -> Sen e' b
  }

instance Category (~:>) where
  id = DocMap { overSec = id, overBlk = id, overInl = id, overSen = id }
  a . b = DocMap 
    { overSec = overSec a . overSec b
    , overBlk = overBlk a . overBlk b
    , overInl = overInl a . overInl b
    , overSen = overSen a . overSen b
    }

-- | Unfix is extracted version of a DocR e
data Unfix e

instance DocR (Unfix e) where
  type Sec (Unfix e) = Section e
  type Blk (Unfix e) = Block e
  type Inl (Unfix e) = Inline e
  type Sen (Unfix e) = Sentence e

hylo :: forall e e'. e ~:> Unfix e -> Unfix e' ~:> e' -> e ~:> e'
hylo project embed = extract
 where 
  extract = DocMap
    { overSec = overSec embed . mapDoc extract . overSec project 
    , overBlk = overBlk embed . mapDoc extract . overBlk project
    , overInl = overInl embed . mapDoc extract . overInl project
    , overSen = overSen embed . mapDocSen extract . overSen project
    }

class DocR e => ProjectableR e where
  projectR :: AnaR e

type CataR a = Unfix a ~:> a

cata :: forall e a. ProjectableR e => Unfix a ~:> a -> e ~:> a 
cata = hylo projectR

class DocR e => EmbedableR e where
  embedR :: CataR e

pureR :: Applicative m => e ~:> Monadic e m
pureR = DocMap
  { overSec = pure
  , overBlk = pure
  , overInl = pure
  , overSen = \e -> MonadicSen $ pure e
  }

embedRM :: (Applicative m, EmbedableR e) => Unfix e ~:> Monadic e m
embedRM = pureR . embedR 

type AnaR a = a ~:> Unfix a

ana :: forall e a. EmbedableR e => a ~:> Unfix a -> a ~:> e
ana da = hylo da embedR



data Monadic e (m :: * -> *)

newtype MonadicSen e (m :: * -> *) (b :: SenType) = MonadicSen 
  { unMonadicSen :: m (Sen e b)
  }

instance DocR (Monadic e m) where
  type Sec (Monadic e m) = m (Sec e) 
  type Blk (Monadic e m) = m (Blk e) 
  type Inl (Monadic e m) = m (Inl e) 
  type Sen (Monadic e m) = MonadicSen e m


hyloM :: forall e e' m. 
  Monad m
  => (forall a. Unfix (Monadic a m) ~:> Monadic (Unfix a) m)
  -> e ~:> Monadic (Unfix e) m 
  -> Unfix e' ~:> Monadic e' m 
  -> e ~:> Monadic e' m
hyloM dist project embed = extract
 where 
  extract :: e ~:> Monadic e' m
  extract = DocMap 
    { overSec = overSec embed <=< overSec dist . mapDoc extract <=<  overSec project
    , overBlk = overBlk embed <=< overBlk dist . mapDoc extract <=<  overBlk project
    , overInl = overInl embed <=< overInl dist . mapDoc extract <=<  overInl project
    , overSen = MonadicSen .  
      ( unMonadicSen . overSen embed 
      <=< unMonadicSen . overSen dist . mapDocSen extract
      <=< unMonadicSen . overSen project
      )
    }

-- Algebra

class DocFunctor f where
  mapDoc :: e ~:> e' -> f e -> f e'

instance DocFunctor Section where
  mapDoc e Section {..} = Section 
    (mapDoc e sectionTitle)
    (overBlk e <$> sectionContent)
    (overSec e <$> sectionSubs)

instance DocFunctor Sentences where
  mapDoc e = \case
    OpenSentences a -> 
      OpenSentences (overSen e a)
    ClosedSentences a b -> 
      ClosedSentences (overSen e a) (mapDoc e <$> b)

instance DocFunctor Block where
  mapDoc e = \case 
    Para a -> Para (mapDoc e a)

instance DocFunctor Inline where
  mapDoc e = \case
    Qouted (QoutedSentences a b) 
      -> Qouted (QoutedSentences a (mapDoc e b))
    a -> unsafeCoerce a 

instance DocFunctor QoutedSentences where
  mapDoc e (QoutedSentences {..}) = QoutedSentences 
    { qoutedSentences = mapDoc e qoutedSentences
    , ..
    }

instance DocFunctor Item where
  mapDoc e Item {..} = Item 
    { itemTitle = mapDoc e itemTitle
    , itemContents = overBlk e <$> itemContents  
    , ..
    }

instance DocFunctor OrderedItem where
  mapDoc e OrderedItem {..} = OrderedItem
    { orderedItemTitle = mapDoc e orderedItemTitle
    , orderedItemContents = overBlk e <$> orderedItemContents 
    , ..
    }

mapDocSen :: e ~:> e' -> Sentence e a -> Sentence e' a
mapDocSen e = \case
  OpenSentence ins -> OpenSentence (overInl e <$> ins)
  ClosedSentence ins ends -> ClosedSentence (overInl e <$> ins) ends


data Extractor e a = Extractor
  { fromSec :: Sec e -> a
  , fromBlk :: Blk e -> a
  , fromInl :: Inl e -> a
  , fromSen :: forall b. Sen e b -> a
  } deriving (Functor) 

data Value a
newtype SenValue a (b :: SenType) = 
  SenValue { unSenValue :: a }

instance DocR (Value a) where
  type Sec (Value a) = a
  type Blk (Value a) = a
  type Inl (Value a) = a
  type Sen (Value a) = SenValue a

data DocAlgebra e a = DocAlgebra
  { fromSection :: Section e -> a
  , fromBlock :: Block e -> a
  , fromInline :: Inline e -> a
  , fromItem :: Item e -> a
  , fromOrderedItem :: OrderedItem e -> a
  , fromQoutedSentences :: QoutedSentences e -> a
  , fromSentences :: Sentences e -> a
  , fromSentence :: forall b. Sentence e b -> a
  } deriving (Functor)

contramapAlgebra :: e ~:> e' -> DocAlgebra e' a -> DocAlgebra e a
contramapAlgebra e DocAlgebra {..} = DocAlgebra
  { fromSection = fromSection . mapDoc e
  , fromBlock = fromBlock . mapDoc e
  , fromInline = fromInline . mapDoc e
  , fromItem = fromItem . mapDoc e
  , fromOrderedItem = fromOrderedItem . mapDoc e
  , fromQoutedSentences = fromQoutedSentences . mapDoc e
  , fromSentences = fromSentences . mapDoc e
  , fromSentence = fromSentence . mapDocSen e
  }

contramapExtractor :: forall e e' a. e ~:> e' -> Extractor e' a -> Extractor e a
contramapExtractor fn Extractor {..} = Extractor
  { fromSec = fromSec . overSec fn 
  , fromBlk = fromBlk . overBlk fn
  , fromInl = fromInl . overInl fn
  , fromSen = fromSen . overSen fn
  }

-- | A default fold, should be overloaded
foldDoc :: forall m. Monoid m => DocAlgebra (Value m) m
foldDoc = DocAlgebra {..}
 where
  fromSection Section {..} =
    fromSentences sectionTitle
    <> fold sectionContent
    <> fold sectionSubs

  fromBlock = \case
    Para s -> fromSentences s
    Comment _ -> mempty
    Items its -> 
      foldMap fromItem its
    OrderedItems _ its -> 
      foldMap fromOrderedItem its

  fromItem Item {..} =
    fromSentences itemTitle 
    <> fold itemContents

  fromOrderedItem OrderedItem {..} = 
    fromSentences orderedItemTitle
    <> fold orderedItemContents

  fromInline = \case
    Qouted a -> fromQoutedSentences a
    _ -> mempty

  fromQoutedSentences QoutedSentences {..} =
    fromSentences qoutedSentences

  fromSentences = \case
    OpenSentences (SenValue sen) -> sen
    ClosedSentences (SenValue sen) rest -> sen <> foldMap fromSentences rest

  fromSentence :: forall b. Sentence (Value m) b -> m
  fromSentence = \case
    OpenSentence wrds -> fold wrds
    ClosedSentence wrds end -> fold wrds


fromAlgebra :: DocAlgebra e a -> Unfix e ~:> Value a
fromAlgebra pj = DocMap
  { overSec = fromSection pj
  , overBlk = fromBlock pj
  , overInl = fromInline pj
  , overSen = SenValue . fromSentence pj
  }

toExtractor :: e ~:> Value a -> Extractor e a
toExtractor DocMap {..} = Extractor
  { fromSec = overSec
  , fromBlk = overBlk
  , fromInl = overInl
  , fromSen = \a -> unSenValue (overSen a)
  }

data GeneratorM e m = GeneratorM
  { toSec :: m (Sec e)
  , toBlk :: m (Blk e)
  , toInl :: m (Inl e)
  , toSen :: m (AnySen e)
  } 

toGeneratorM :: Value a ~:> Monadic e m -> GeneratorM e (ReaderT a m)
toGeneratorM DocMap {..} = GeneratorM
  { toSec = ReaderT overSec
  , toBlk = ReaderT overBlk
  , toInl = ReaderT overInl
  , toSen = ReaderT (\a -> unMonadicSen (overSen (SenValue a)))
  }

countSentences :: DocAlgebra (Value (Sum Int)) (Sum Int)
countSentences = foldDoc { fromSentence = const 1 }

countWords :: DocAlgebra (Value (Sum Int)) (Sum Int)
countWords = fd { fromInline = \case
    Word _ -> 1
    Number _ -> 1
    Verbatim _ -> 1
    a -> fromInline fd a
  }
 where fd = foldDoc

-- newtype ItemTree i = ItemTree (Item (ItemTree i, i))
-- 
-- compressItem :: (b -> Maybe (NE.NonEmpty (Item b i))) -> Item b i -> Maybe (ItemTree i)
-- compressItem fn Item {..} = ItemTree <$> do
--   contents <- case itemContents of
--     [c] -> fmap NE.toList . mapM (compressItem fn) =<< fn c
--     [] -> pure []
--     _ -> Nothing
--   pure $ Item
--     { itemType = itemType
--     , itemTodo = itemTodo
--     , itemTitle = itemTitle
--     , itemContents = contents
--     }
-- 
-- compressItem' :: Item Block' Inline' -> Maybe (ItemTree Inline')
-- compressItem' = compressItem (\case
--   Block' (Items it) -> Just it
--   _ -> Nothing
--   )

instance DocR (a, b) where 
  type Sec (a, b) = Void
  type Inl (a, b) = b
  type Blk (a, b) = a
  type Sen (a, b) = Sentence (a, b)


-- TODO
sequenceR :: forall m a. Monad m => Unfix (Monadic a m) ~:> Monadic (Unfix a) m
sequenceR = DocMap {..}
 where 
  overSec Section {..} = do
    sectionTitle' <- overSentences sectionTitle
    sectionContent' <- sequence sectionContent
    sectionSubs' <- sequence sectionSubs
    return Section
      { sectionTitle = sectionTitle'
      , sectionContent = sectionContent'
      , sectionSubs = sectionSubs'
      }

  overBlk = \case 
    Para a -> Para <$> overSentences a
    Comment m -> pure $ Comment m

  overInl = \case
    Qouted q -> Qouted <$> overQoutedSentences q
    -- a -> pure $ unsafeCoerce a

  overSen :: forall b. Sentence (Monadic a m) b -> MonadicSen (Unfix a) m b
  overSen = \case 
    OpenSentence sen -> 
      MonadicSen $ OpenSentence <$> sequence sen
    ClosedSentence sen end -> 
      MonadicSen $ ClosedSentence <$> sequence sen <*> pure end

  overQoutedSentences QoutedSentences {..} = do
    qoutedSentences' <- overSentences qoutedSentences
    return QoutedSentences 
      { qoutedSentences = qoutedSentences'
      , ..
      }

  overSentences = \case 
    OpenSentences sen -> 
      OpenSentences <$> unMonadicSen sen 
    ClosedSentences sen rest -> 
      ClosedSentences <$> unMonadicSen sen <*> traverse overSentences rest
