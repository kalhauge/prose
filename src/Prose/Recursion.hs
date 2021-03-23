{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Prose.Recursion where

-- base
import Unsafe.Coerce
import Data.Foldable
import Data.Monoid
import Data.Functor.Identity
import Control.Category
import Prelude hiding (Word, (.), id)

-- mtl
import Control.Monad.Reader


import Prose.Doc

-- | Instance is an cool thing
data Instance e = Instance 
  { onSec :: Sec e
  , onBlk :: Blk e
  , onInl :: Inl e
  , onOpenSen :: OpenSen e
  , onClosedSen :: ClosedSen e
  }

-- Value
data Value a 

instance DocR (Value a) where
  type Sec (Value a) = a
  type Blk (Value a) = a
  type Inl (Value a) = a
  type OpenSen (Value a) = a
  type ClosedSen (Value a) = a

valueR :: a -> Instance (Value a)
valueR a = Instance a a a a a

-- | Map
newtype e ~:> e' = DocMap { docMapper :: Instance (e ~:> e') }

instance DocR (e ~:> e') where
  type Sec (e ~:> e') = Sec e -> Sec e'
  type Blk (e ~:> e') = Blk e -> Blk e'
  type Inl (e ~:> e') = Inl e -> Inl e'
  type OpenSen (e ~:> e') = OpenSen e -> OpenSen e'
  type ClosedSen (e ~:> e') = ClosedSen e -> ClosedSen e'

overSec :: e ~:> e' -> Sec e -> Sec e'
overSec = onSec . docMapper

overBlk :: e ~:> e' -> Blk e -> Blk e'
overBlk = onBlk . docMapper

overInl :: e ~:> e' -> Inl e -> Inl e'
overInl = onInl . docMapper

overOpenSen :: e ~:> e' -> OpenSen e -> OpenSen e'
overOpenSen = onOpenSen . docMapper

overClosedSen :: e ~:> e' -> ClosedSen e -> ClosedSen e'
overClosedSen = onClosedSen . docMapper

class DocFunctor f where
  mapDoc :: e ~:> e' -> f e -> f e'

instance DocFunctor Instance where
  mapDoc (DocMap fn) ins = Instance
    { onSec = onSec fn $ onSec ins
    , onBlk = onBlk fn $ onBlk ins
    , onInl = onInl fn $ onInl ins
    , onOpenSen = onOpenSen fn $ onOpenSen ins
    , onClosedSen = onClosedSen fn $ onClosedSen ins
    }

instance Category (~:>) where
  id = DocMap $ Instance id id id id id
  DocMap a . DocMap b = DocMap $ Instance 
    { onSec = onSec a . onSec b
    , onBlk = onBlk a . onBlk b
    , onInl = onInl a . onInl b
    , onOpenSen = onOpenSen a . onOpenSen b
    , onClosedSen = onClosedSen a . onClosedSen b
    }

data Apply (m :: * -> *) e

type Monadic m e = Instance (Apply m e)

instance DocR (Apply m e) where
  type Sec (Apply m e) = m (Sec e)
  type Blk (Apply m e) = m (Blk e)
  type Inl (Apply m e) = m (Inl e)
  type OpenSen (Apply m e) = m (OpenSen e)
  type ClosedSen (Apply m e) = m (ClosedSen e)

mapV :: (a -> b) -> Value a ~:> Value b
mapV fn = DocMap $ Instance fn fn fn fn fn

liftR :: forall e m. (forall a. a -> m a) -> e ~:> Apply m e
liftR fn = DocMap $ Instance fn fn fn fn fn

unliftR :: forall e m. (forall a. m a -> a) -> Apply m e ~:> e 
unliftR fn = DocMap $ Instance fn fn fn fn fn

pureR :: Applicative m => e ~:> Apply m e
pureR = liftR pure

natR :: forall e m m'. (forall a. m a -> m' a) -> Apply m e ~:> Apply m' e
natR fn = DocMap $ Instance fn fn fn fn fn

runIdentityR :: Apply Identity e ~:> e
runIdentityR = unliftR runIdentity

runReaderTR :: r -> Apply (ReaderT r m) e ~:> Apply m e
runReaderTR r = natR (`runReaderT` r)

runReaderR :: r -> Apply (Reader r) e ~:> e
runReaderR r = runIdentityR . runReaderTR r

runReaderR' :: r -> Apply ((->) r) e ~:> e
runReaderR' r = unliftR ($ r)

-- Unfix

-- | Unfix is extracted version of a DocR e
data Unfix e

instance DocR (Unfix e) where
  type Sec (Unfix e) = Section e
  type Blk (Unfix e) = Block e
  type Inl (Unfix e) = Inline e
  type OpenSen (Unfix e) = Sentence 'Open e
  type ClosedSen (Unfix e) = Sentence 'Closed e

hylo :: 
  e ~:> Unfix e 
  -> Unfix e' ~:> e' 
  -> e ~:> e'
hylo project embed = extract
 where 
  extract = DocMap $ Instance
    { onSec = overSec embed . mapDoc extract . overSec project 
    , onBlk = overBlk embed . mapDoc extract . overBlk project
    , onInl = overInl embed . mapDoc extract . overInl project
    , onOpenSen = overOpenSen embed . mapDocSen extract . overOpenSen project
    , onClosedSen = overClosedSen embed . mapDocSen extract . overClosedSen project
    }

class DocR e => ProjectableR e where
  projectR :: e ~:> Unfix e

class DocR e => EmbedableR e where
  embedR :: Unfix e ~:> e

embedRM :: (EmbedableR e, Applicative m) 
  => Unfix e ~:> Apply m e
embedRM = pureR . embedR

cata :: ProjectableR e 
  => Unfix a ~:> a 
  -> e ~:> a 
cata = hylo projectR

ana :: EmbedableR e
  => a ~:> Unfix a 
  -> a ~:> e
ana = flip hylo embedR

hyloM :: Monad m
  => (forall a. Unfix (Apply m a) ~:> Apply m (Unfix a))
  -> e ~:> Apply m (Unfix e) 
  -> Unfix e' ~:> Apply m e'
  -> e ~:> Apply m e'
hyloM dist project embed = extract
 where 
  extract = DocMap $ Instance
    { onSec = 
        overSec embed 
        <=< overSec dist . mapDoc extract 
        <=< overSec project
    , onBlk = 
        overBlk embed 
        <=< overBlk dist . mapDoc extract 
        <=< overBlk project
    , onInl = 
        overInl embed 
        <=< overInl dist . mapDoc extract 
        <=< overInl project
    , onOpenSen = 
        overOpenSen embed 
        <=< overOpenSen dist . mapDocSen extract
        <=< overOpenSen project
    , onClosedSen = 
        overClosedSen embed 
        <=< overClosedSen dist . mapDocSen extract
        <=< overClosedSen project
    }

-- Instances

instance DocFunctor Section where
  mapDoc e Section {..} = Section 
    (mapDoc e sectionTitle)
    (overBlk e <$> sectionContent)
    (overSec e <$> sectionSubs)

instance DocFunctor Sentences where
  mapDoc e = \case
    OpenSentences a -> 
      OpenSentences (overOpenSen e a)
    ClosedSentences a b -> 
      ClosedSentences (overClosedSen e a) (mapDoc e <$> b)

instance DocFunctor Block where
  mapDoc e = \case 
    Para a -> Para (mapDoc e a)
    Items itm -> Items (mapDoc e <$> itm)
    Comment a -> Comment a
    OrderedItems n itm -> OrderedItems n (mapDoc e <$> itm)

instance DocFunctor Inline where
  mapDoc e = \case
    Qouted (QoutedSentences a b) 
      -> Qouted (QoutedSentences a (mapDoc e b))
    a -> unsafeCoerce a 

instance DocFunctor QoutedSentences where
  mapDoc e QoutedSentences {..} = QoutedSentences 
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

mapDocSen :: e ~:> e' -> Sentence a e -> Sentence a e'
mapDocSen e = \case
  OpenSentence ins -> OpenSentence (overInl e <$> ins)
  ClosedSentence ins ends -> ClosedSentence (overInl e <$> ins) ends

data DocAlgebra e a = DocAlgebra
  { fromSection :: Section e -> a
  , fromBlock :: Block e -> a
  , fromInline :: Inline e -> a
  , fromItem :: Item e -> a
  , fromOrderedItem :: OrderedItem e -> a
  , fromQoutedSentences :: QoutedSentences e -> a
  , fromSentences :: Sentences e -> a
  , fromSentence :: forall b. Sentence b e -> a
  } deriving (Functor)

type Extractor e v = e ~:> Value v

extractR :: forall e a. ProjectableR e 
  => DocAlgebra (Value a) a 
  -> e ~:> Value a
extractR DocAlgebra {..} = cata $ DocMap $ Instance 
  fromSection 
  fromBlock 
  fromInline 
  fromSentence 
  fromSentence

cataA :: forall e a. 
  ProjectableR e
  => DocAlgebra (Value a) a 
  -> DocAlgebra e a
cataA alg = extractR alg `contramapAlgebra` alg


foldR :: forall e v. 
     (e ~:> Unfix e)
  -> (Extractor e v -> DocAlgebra e v) 
  -> Extractor e v
foldR project fn = extractor . project
 where
  DocAlgebra {..} = fn (extractor . project)
  -- gen = mapDoc (joinR . underR emb) generator
  extractor :: Extractor (Unfix e) v
  extractor = DocMap $ Instance 
   { onSec = fromSection
   , onBlk = fromBlock
   , onInl = fromInline
   , onOpenSen = fromSentence
   , onClosedSen = fromSentence
   }

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
    OpenSentences sen -> sen
    ClosedSentences sen rest -> sen <> foldMap fromSentences rest

  fromSentence :: Sentence b (Value m) -> m
  fromSentence = \case
    OpenSentence wrds -> fold wrds
    ClosedSentence wrds _ -> fold wrds

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


data DocCoAlgebra m e = DocCoAlgebra
  { toSection :: m (Section e)
  , toBlock :: m (Block e)
  , toInline :: m (Inline e)
  , toItem :: m (Item e)
  , toOrderedItem :: m (OrderedItem e)
  , toQoutedSentences :: m (QoutedSentences e)
  , toSentences :: m (Sentences e)
  , toOpenSentence :: m (Sentence 'Open e)
  , toClosedSentence :: m (Sentence 'Closed e)
  } 

natCoAlgebra :: 
  (forall a. m a -> m' a) 
  -> DocCoAlgebra m e 
  -> DocCoAlgebra m' e 
natCoAlgebra fn DocCoAlgebra {..} = DocCoAlgebra 
  { toSection = fn toSection
  , toBlock = fn toBlock
  , toInline = fn toInline
  , toItem = fn toItem
  , toOrderedItem = fn toOrderedItem
  , toSentences = fn toSentences
  , toQoutedSentences = fn toQoutedSentences
  , toOpenSentence = fn toOpenSentence
  , toClosedSentence = fn toClosedSentence
  }

instance Functor m => DocFunctor (DocCoAlgebra m) where
  mapDoc fn DocCoAlgebra {..} = DocCoAlgebra 
    { toSection = mapDoc fn <$> toSection
    , toBlock = mapDoc fn <$> toBlock
    , toInline = mapDoc fn <$> toInline
    , toItem = mapDoc fn <$> toItem
    , toOrderedItem = mapDoc fn <$> toOrderedItem
    , toSentences = mapDoc fn <$> toSentences
    , toQoutedSentences = mapDoc fn <$> toQoutedSentences
    , toOpenSentence = mapDocSen fn <$> toOpenSentence
    , toClosedSentence = mapDocSen fn <$> toClosedSentence
    }

type Generator m e = Monadic m e 

joinR :: Monad m => Apply m (Apply m e) ~:> Apply m e
joinR = DocMap $ Instance join join join join join

underR :: Functor m => (a ~:> b) -> (Apply m a ~:> Apply m b)
underR amb = DocMap $ Instance 
  { onSec = fmap $ overSec amb 
  , onBlk = fmap $ overBlk amb 
  , onInl = fmap $ overInl amb 
  , onOpenSen = fmap $ overOpenSen amb 
  , onClosedSen = fmap $ overClosedSen amb 
  }

bindR :: Monad m 
  => (a ~:> Apply m b) 
  -> (b ~:> Apply m c) 
  -> (a ~:> Apply m c)
bindR amb bmc = DocMap $ Instance 
  { onSec = overSec amb >=> overSec bmc 
  , onBlk = overBlk amb >=> overBlk bmc
  , onInl = overInl amb >=> overInl bmc
  , onOpenSen = overOpenSen amb >=> overOpenSen bmc
  , onClosedSen = overClosedSen amb >=> overClosedSen bmc
  }

anaA :: forall e m. 
  Monad m 
  => (Unfix e ~:> Apply m e)
  -> (Generator m e -> DocCoAlgebra m e) 
  -> Generator m e
anaA emb fn = gen
 where
  DocCoAlgebra {..} = fn gen
  gen = mapDoc (joinR . underR emb) generator
  generator = Instance 
   { onSec = toSection
   , onBlk = toBlock
   , onInl = toInline
   , onOpenSen = toOpenSentence
   , onClosedSen = toClosedSentence
   }


-- fromAlgebra :: DocAlgebra e a -> Unfix e ~:> Value a
-- fromAlgebra pj = DocMap
--   { overSec = fromSection pj
--   , overBlk = fromBlock pj
--   , overInl = fromInline pj
--   , overSen = SenValue . fromSentence pj
--   }
-- 
-- toExtractor :: e ~:> Value a -> Extractor e a
-- toExtractor DocMap {..} = Extractor
--   { fromSec = overSec
--   , fromBlk = overBlk
--   , fromInl = overInl
--   , fromSen = unSenValue . overSen
--   }
-- 
-- data GeneratorM e m = GeneratorM
--   { toSec :: m (Sec e)
--   , toBlk :: m (Blk e)
--   , toInl :: m (Inl e)
--   , toSen :: m (AnySen e)
--   } 
-- 
-- toGeneratorM :: Monad m => Value a ~:> Monadic e m -> GeneratorM e (ReaderT a m)
-- toGeneratorM DocMap {..} = GeneratorM
--   { toSec = ReaderT overSec
--   , toBlk = ReaderT overBlk
--   , toInl = ReaderT overInl
--   , toSen = ReaderT (\a -> AnySen <$> unMonadicSen (overSen (SenValue a)))
--   }
-- 

-- -- instance DocR (a, b) where 
-- --   type Sec (a, b) = Void
-- --   type Inl (a, b) = b
-- --   type Blk (a, b) = a
-- --   type Sen (a, b) = Sentence (a, b)
-- 
-- 
-- -- TODO
-- -- sequenceR :: forall m a. Monad m => Unfix (Monadic a m) ~:> Monadic (Unfix a) m
-- -- sequenceR = DocMap {..}
-- --  where 
-- --   overSec Section {..} = do
-- --     sectionTitle' <- overSentences sectionTitle
-- --     sectionContent' <- sequence sectionContent
-- --     sectionSubs' <- sequence sectionSubs
-- --     return Section
-- --       { sectionTitle = sectionTitle'
-- --       , sectionContent = sectionContent'
-- --       , sectionSubs = sectionSubs'
-- --       }
-- -- 
-- --   overBlk = \case 
-- --     Para a -> Para <$> overSentences a
-- --     Comment m -> pure $ Comment m
-- -- 
-- --   overInl = \case
-- --     Qouted q -> Qouted <$> overQoutedSentences q
-- --     a -> pure $ unsafeCoerce a
-- -- 
-- --   overSen :: forall b. Sentence (Monadic a m) b -> MonadicSen (Unfix a) m b
-- --   overSen = \case 
-- --     OpenSentence sen -> 
-- --       MonadicSen $ OpenSentence <$> sequence sen
-- --     ClosedSentence sen end -> 
-- --       MonadicSen $ ClosedSentence <$> sequence sen <*> pure end
-- -- 
-- --   overQoutedSentences QoutedSentences {..} = do
-- --     qoutedSentences' <- overSentences qoutedSentences
-- --     return QoutedSentences 
-- --       { qoutedSentences = qoutedSentences'
-- --       , ..
-- --       }
-- -- 
-- --   overSentences = \case 
-- --     OpenSentences sen -> 
-- --       OpenSentences <$> unMonadicSen sen 
-- --     ClosedSentences sen rest -> 
-- --       ClosedSentences <$> unMonadicSen sen <*> traverse overSentences rest
