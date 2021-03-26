{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Prose.Doc where

-- base
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import Prelude hiding (Word)

-- text
import Data.Text qualified as Text

data SenType = Open | Closed

-- | To make documents extenable we define the things that exist recursively.
class DocR e where
  type Sec e :: *
  type Blk e :: *
  type Inl e :: *
  type OpenSen e :: *
  type ClosedSen e :: *

class 
  ( DocR e
  , Show (Sec e)
  , Show (Blk e)
  , Show (Inl e)
  , Show (OpenSen e)
  , Show (ClosedSen e)
  ) => ShowR e where

class 
  ( DocR e
  , Eq (Sec e)
  , Eq (Blk e)
  , Eq (Inl e)
  , Eq (OpenSen e)
  , Eq (ClosedSen e)
  ) => EqR e where
class 
  ( EqR e
  , Ord (Sec e)
  , Ord (Blk e)
  , Ord (Inl e)
  , Ord (OpenSen e)
  , Ord (ClosedSen e)
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
  = OpenSentences (OpenSen e)
  | ClosedSentences (ClosedSen e) (Maybe (Sentences e))

deriving instance EqR e => Eq (Sentences e)
deriving instance OrdR e => Ord (Sentences e)

instance ShowR e => Show (Sentences e) where
  showsPrec n s =
    showParen (n > 10) $ showString "sen" . showsPrec 11 (toSentenceBuilder s)

-- data AnySen e = forall b. AnySen { getAnySen :: Sen e b }

data Sentence a e where
  ClosedSentence   :: NE.NonEmpty (Inl e) -> NE.NonEmpty End -> Sentence 'Closed e
  OpenSentence :: NE.NonEmpty (Inl e) -> Sentence 'Open e

deriving instance EqR e => Eq (Sentence a e)
deriving instance OrdR e => Ord (Sentence a e)
deriving instance ShowR e => Show (Sentence a e)

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
  = Word !Text.Text
  | Reference !Text.Text
  | Mark !Mark
  | Verbatim !Text.Text
  | Number !Text.Text
  | Qouted (QoutedSentences e)

deriving instance EqR e => Eq (Inline e)
deriving instance OrdR e => Ord (Inline e)
deriving instance ShowR e => Show (Inline e)

data Mark 
  = Comma
  | Colon
  | SemiColon
  deriving (Eq, Show, Ord, Enum, Bounded)

fromSentenceBuilder :: forall e. SentenceBuilder e -> Sentences e
fromSentenceBuilder = \case
  Current ne lst ->
    case NE.nonEmpty lst of
      Just x -> go (Just $ OpenSentences ne) x
      Nothing -> OpenSentences ne
  AllClosed rest ->
    go Nothing rest
 where
  go :: Maybe (Sentences e) -> NE.NonEmpty (ClosedSen e) -> Sentences e
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
  Current (OpenSen e) [ClosedSen e]
  | AllClosed (NE.NonEmpty (ClosedSen e))

deriving instance EqR e => Eq (SentenceBuilder e)
deriving instance OrdR e => Ord (SentenceBuilder e)
deriving instance ShowR e => Show (SentenceBuilder e)





