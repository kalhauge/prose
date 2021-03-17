{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
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
module Prose.Doc where

-- base
import GHC.Generics (Generic)
import Text.Show
import Data.Void
import Data.Monoid
import Data.Foldable
import Prelude hiding (Word)
import Data.List.NonEmpty qualified as NE

-- text
import Data.Text qualified as Text

-- | To make documents extenable we define the things that exist recursively.
class DocR e where
  type Sec e :: *
  type Blk e :: *
  type Inl e :: *
  type Sen e :: SenType -> *

class 
  ( DocR e
  , Show (Sec e)
  , Show (Blk e)
  , Show (Inl e)
  , Show (Sen e 'Open)
  , Show (Sen e 'Closed)
  ) => ShowR e where

class 
  ( DocR e
  , Eq (Sec e)
  , Eq (Blk e)
  , Eq (Inl e)
  , Eq (Sen e 'Open)
  , Eq (Sen e 'Closed)
  ) => EqR e where
class 
  ( EqR e
  , Ord (Sec e)
  , Ord (Blk e)
  , Ord (Inl e)
  , Ord (Sen e 'Open)
  , Ord (Sen e 'Closed)
  ) => OrdR e where

-- | A Section
data Section e = Section
  { sectionTitle :: Sentences e
  , sectionContent :: [Blk e]
  , sectionSubs :: [Sec e]
  } 
  deriving (Generic)

deriving instance EqR e => Eq (Section e)
deriving instance OrdR e => Ord (Section e)
deriving instance ShowR e => Show (Section e)

data Block e
  = Para (Sentences e)
  | Comment [Text.Text]
  | Items (NE.NonEmpty (Item e))
  | OrderedItems OrderType (NE.NonEmpty (OrderedItem e))

deriving instance EqR e => Eq (Block e)
deriving instance OrdR e => Ord (Block e)
deriving instance ShowR e => Show (Block e)

data Item e = Item
  { itemType :: ItemType
  , itemTodo :: Maybe Bool
  , itemTitle :: Sentences e
  , itemContents :: [Blk e]
  }

deriving instance EqR e => Eq (Item e)
deriving instance OrdR e => Ord (Item e)
deriving instance ShowR e => Show (Item e)

data ItemType
  = Minus
  | Plus
  | Times
  deriving (Eq, Show, Enum, Bounded, Ord)

data OrderedItem e = OrderedItem
  { orderedItemTodo :: Maybe Bool
  , orderedItemTitle :: Sentences e
  , orderedItemContents :: [Blk e]
  }

deriving instance EqR e => Eq (OrderedItem e)
deriving instance OrdR e => Ord (OrderedItem e)
deriving instance ShowR e => Show (OrderedItem e)

data OrderType
  = Numeral
  deriving (Eq, Show, Enum, Bounded, Ord)

data Sentences e
  = OpenSentences (Sen e 'Open)
  | ClosedSentences (Sen e 'Closed) (Maybe (Sentences e))

deriving instance EqR e => Eq (Sentences e)
deriving instance OrdR e => Ord (Sentences e)

instance ShowR e => Show (Sentences e) where
  showsPrec n s =
    showParen (n > 10) $ showString "sen" . showsPrec 11 (toSentenceBuilder s)

-- instance Functor Sentences where
--   fmap fn = \case
--     OpenSentence n ->
--       OpenSentence (fmap fn n)
--     ClosedSentence sn ms->
--       ClosedSentence (fmap fn sn) (fmap (fmap fn) ms)

data SenType = Open | Closed

data Sentence e a where
  ClosedSentence   :: NE.NonEmpty (Inl e) -> NE.NonEmpty End -> Sentence e 'Closed 
  OpenSentence :: NE.NonEmpty (Inl e) -> Sentence e 'Open 

deriving instance EqR e => Eq (Sentence e a)
deriving instance OrdR e => Ord (Sentence e a)
deriving instance ShowR e => Show (Sentence e a)

data End
  = Exclamation
  | Question
  | Period
  deriving (Eq, Show, Enum, Bounded, Ord)

data QoutedSentences e = QoutedSentences
  { qoutedType :: Qoute
  , qoutedSentences :: Sentences e
  }

deriving instance EqR e => Eq (QoutedSentences e)
deriving instance OrdR e => Ord (QoutedSentences e)
deriving instance ShowR e => Show (QoutedSentences e)

data Qoute
  = DoubleQoute
  | Brackets
  | Parenthesis
  | Emph
  | Strong
  deriving (Eq, Show, Enum, Bounded, Ord)

data Inline e
  = Word Text.Text
  | Reference Text.Text
  | Comma
  | Colon
  | SemiColon
  | Verbatim Text.Text
  | Number Text.Text
  | Qouted (QoutedSentences e)

deriving instance EqR e => Eq (Inline e)
deriving instance OrdR e => Ord (Inline e)
deriving instance ShowR e => Show (Inline e)


-- Algebra

-- | Unfix is extracted version of a DocR e
data Unfix e

instance DocR (Unfix e) where
  type Sec (Unfix e) = Section e
  type Blk (Unfix e) = Block e
  type Inl (Unfix e) = Inline e
  type Sen (Unfix e) = Sentence e


data Extractor e e' = Extractor
  { fromSec :: Sec e -> Sec e'
  , fromBlk :: Blk e -> Blk e'
  , fromInl :: Inl e -> Inl e'
  , fromSen :: forall b. Sen e b -> Sen e' b
  } 

class DocMap f where
  mapDoc :: (DocR e, DocR e') => Extractor e e' -> f e -> f e'

instance DocMap Section where
  mapDoc e Section {..} = Section 
    (mapDoc e sectionTitle)
    (fromBlk e <$> sectionContent)
    (fromSec e <$> sectionSubs)

instance DocMap Sentences where
  mapDoc e = \case
    OpenSentences a -> 
      OpenSentences (fromSen e a)
    ClosedSentences a b -> 
      ClosedSentences (fromSen e a) (mapDoc e <$> b)

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
  , fromSentences :: Sentences e -> a
  , fromSentence :: forall b. Sentence e b -> a
  } deriving (Functor)

-- mapAlgebra :: Extractor e e' -> DocAlgebra e a -> DocAlgebra e' a
-- mapAlgebra ex e = DocAlgebra fromSection'
--  where
--   fromSection' = fromSection e

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
    Qouted (QoutedSentences _ sen) -> 
      fromSentences sen
    _ -> mempty

  fromSentences = \case
    OpenSentences (SenValue sen) -> sen
    ClosedSentences (SenValue sen) rest -> sen <> foldMap fromSentences rest

  fromSentence :: forall b. Sentence (Value m) b -> m
  fromSentence = \case
    OpenSentence wrds -> fold wrds
    ClosedSentence wrds end -> fold wrds

countSentences :: DocAlgebra (Value (Sum Int)) (Sum Int)
countSentences = foldDoc { fromSentence = const 1 }

-- simplify :: forall a. DocAlgebra a -> Extractor Simple (DAlgebra a)
-- simplify a = DocAlgebra {..}
--  where 
--   fromSec (Section' sec) = fromSection a sec
--   fromBlk (Block' blk) = fromBlock a blk
--   fromInl (Inline' inl) = fromInline a inl
--   fromSen :: forall b. Sentence Simple b -> a
--   fromSen sen = fromSentence a sen
-- 
-- countSimpleSentences :: Extractor Simple (DAlgebra Int)
-- countSimpleSentences = getSum <$> result
--   where result = simplify (countSentences result)

-- countSentencesInInline :: Inline' -> Int
-- countSentencesInInline (Inline' inline) = case inline of
--   Qouted q -> countSentencesInQouted countSentencesInInline q
--   _ -> 0
-- 
-- countSentences :: (i -> Int) -> Sentences i -> Int
-- countSentences cnt = \case
--   OpenSentence x ->
--     1 + sum (fmap cnt x)
--   ClosedSentence (Sentence is _) rest ->
--     1 + sum (fmap cnt is) + maybe 0 (countSentences cnt) rest
-- 
-- 
-- countSentencesInQouted :: (i -> Int) -> QoutedSentences i -> Int
-- countSentencesInQouted count (QoutedSentences _ s) =
--   countSentences count s

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


data Simple

instance DocR (a, b) where 
  type Sec (a, b) = Void
  type Inl (a, b) = b
  type Blk (a, b) = a
  type Sen (a, b) = Sentence (a, b)

instance DocR Simple where 
  type Sec Simple = Section'
  type Inl Simple = Inline'
  type Blk Simple = Block'
  type Sen Simple = Sentence Simple

instance OrdR Simple where
instance EqR Simple where
instance ShowR Simple where

type Doc = Section'

newtype Section' = Section'
  { getSection :: Section Simple }
  deriving (Eq, Ord, Show)

newtype Block' = Block'
  { getBlock :: Block Simple }
  deriving (Eq, Ord, Show)

newtype Inline' = Inline'
  { getInline :: Inline Simple }
  deriving (Eq, Ord, Show)

type Item' = Item Simple

sen :: forall e. SentenceBuilder e -> Sentences e
sen = \case
  Current ne lst ->
    case NE.nonEmpty lst of
      Just x -> go (Just $ OpenSentences ne) x
      Nothing -> OpenSentences ne
  AllClosed rest ->
    go Nothing rest
 where
  go :: Maybe (Sentences e) -> NE.NonEmpty (Sen e 'Closed) -> Sentences e
  go x (s NE.:| ss) = case NE.nonEmpty ss of
    Nothing -> ClosedSentences s x
    Just rst-> go (Just $ ClosedSentences s x) rst

toSentenceBuilder :: Sentences e -> SentenceBuilder e
toSentenceBuilder = go []
 where
  go ss = \case
    OpenSentences s -> Current s ss
    ClosedSentences s r -> case r of
      Just r' -> go (s:ss) r'
      Nothing -> AllClosed (s NE.:| ss)


data SentenceBuilder e =
  Current (Sen e 'Open) [Sen e 'Closed]
  | AllClosed (NE.NonEmpty (Sen e 'Closed))

deriving instance EqR e => Eq (SentenceBuilder e)
deriving instance OrdR e => Ord (SentenceBuilder e)
deriving instance ShowR e => Show (SentenceBuilder e)

-- instance ShowR e => Show (SentenceBuilder e) where
--   showsPrec n = showParen (n > app_prec) . \case
--     AllClosed sens -> handleRest sens []
--     Current i lst ->
--       case NE.nonEmpty lst of
--         Nothing -> showString "sb " . shows (NE.toList i)
--         Just sens -> handleRest sens (NE.toList i)
-- 
--    where
--     handleRest :: NE.NonEmpty (Sen e 'Closed) -> [Inl e] -> ShowS
--     handleRest (ClosedSentences i (e NE.:| ends)  NE.:| ns) m =
--       ( case NE.nonEmpty ends of
--         Just moreEnds -> shows (AllClosed (Sentence i moreEnds NE.:| ns))
--         Nothing -> shows (Current i ns)
--       )
--       . showsEnd e
--       . shows m
-- 
--     app_prec = 10
-- 
--     -- go (Sentence s (e NE.:| _) NE.:| rest)= case NE.nonEmpty rest of
--     --  Nothing -> showString "sb " . shows (NE.toList s) . showsEnd e
--     --  Just rest' -> go rest' . shows (NE.toList s) . showsEnd e
-- 
--     showsEnd :: End -> ShowS
--     showsEnd = \case
--      Exclamation -> showString " <! "
--      Question -> showString " <? "
--      Period -> showString " <. "

-- instance Show Section' where
--   showsPrec n (Section' s) =
--     showParen (n > app_prec) (showString "Section' " . showsPrec (app_prec + 1) s)
--    where app_prec = 10
-- 
-- instance Show Block' where
--   showsPrec n (Block' s) = case s of
--     Comment txt -> showParen (n > app_prec) $
--       showString "comment' " . shows (Text.intercalate "\n" txt)
--     Para p -> showParen (n > app_prec) $
--       showString "para' " . showsPrec (app_prec + 1) (toSentenceBuilder p)
--     Items it -> showParen (n > app_prec) $
--       showString "items' " . showListWith showsItem (NE.toList it)
--     OrderedItems ot it -> showParen (n > app_prec) $
--       showString "ordered' " . shows ot
--       . showChar ' ' . showListWith showsOrderedItem (NE.toList it)
--    where
--     app_prec = 10
--     showsItem = \case
--       Item Minus Nothing x b ->
--         showString "item' "
--         . showsPrec (app_prec + 1) (toSentenceBuilder x)
--         . showChar ' ' . shows b
--       Item Minus (Just t) x b ->
--         showString "todo' " . shows t . showChar ' '
--         . showsPrec (app_prec + 1) (toSentenceBuilder x)
--         . showChar ' ' . shows b
--       i -> shows i
--     showsOrderedItem (OrderedItem t x b) = case t of
--       Nothing ->
--         showString "oitem' "
--         . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder x)
--         . showChar ' ' . shows b
--       Just t' ->
--         showString "otodo' " . shows t' . showChar ' '
--         . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder x)
--         . showChar ' ' . shows b
-- 
-- 
-- instance Show Inline' where
--   showsPrec n (Inline' s) = case s of
--     Comma -> showString "comma'"
--     Colon -> showString "colon'"
--     SemiColon -> showString "semicolon'"
--     Word w -> showParen (n > app_prec) $
--         showString "word' " . showsPrec (app_prec + 1) w
--     Verbatim w -> showParen (n > app_prec) $
--         showString "verbatim' " . showsPrec (app_prec + 1) w
--     Number w -> showParen (n > app_prec) $
--         showString "number' " . showsPrec (app_prec + 1) w
--     Reference r -> showParen (n > app_prec) $
--         showString "ref' " . showsPrec (app_prec + 1) r
--     Qouted w ->
--       showsQoutedSentences n w
--    where app_prec = 10
-- 
-- -- | Shows an inline with an Inline in.
-- showsInline :: Int -> Inline Inline' -> ShowS
-- showsInline n = \case
--   Qouted w -> showsQoutedSentences n w
--   a -> showsPrec n a
-- 
-- showsQoutedSentences :: Int -> QoutedSentences Inline' -> ShowS
-- showsQoutedSentences n (QoutedSentences qute x) =
--   showParen (n > app_prec) $ showString str . showsPrec (app_prec + 1) (toSentenceBuilder x)
--  where
--    app_prec = 10
--    str = case qute of
--      Brackets -> "brackets' "
--      Parenthesis -> "parens' "
--      DoubleQoute -> "dqoute' "
--      Emph -> "emph' "
--      Strong -> "strong' "
-- 
-- showsSentences :: Int -> Sentences Inline' -> ShowS
-- showsSentences n s = showParen (n > app_prec) $
--   showString "sen' " . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder s)
--  where app_prec = 10

