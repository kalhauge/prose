{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Prose.Doc where

-- base
import Prelude hiding (Word)
import Data.List.NonEmpty qualified as NE
-- import Text.Show

-- text
import Data.Text qualified as Text

-- | A Section
data Section s b i = Section
  { sectionTitle :: Sentences i
  , sectionContent :: [b]
  , sectionSubs :: [s]
  } deriving (Show, Eq)

-- | A Block
data Block b i
  = Para (Sentences i)
  | Comment [Text.Text]
  | Items (NE.NonEmpty (Item b i))
  deriving (Eq, Show)

data Item b i = Item
  { itemType :: ItemType
  , itemTodo :: Maybe Bool
  , itemTitle :: Sentences i
  , itemContents :: [b]
  }
  deriving (Eq, Show)

data ItemType
  = Minus
  | Plus
  | Times
  deriving (Eq, Show, Enum, Bounded)

data Sentences i
  = OpenSentence (NE.NonEmpty i)
  | ClosedSentence
      (Sentence i)
      (Maybe (Sentences i))
  deriving (Eq, Show)

instance Functor Sentences where
  fmap fn = \case
    OpenSentence n ->
      OpenSentence (fmap fn n)
    ClosedSentence sn ms->
      ClosedSentence (fmap fn sn) (fmap (fmap fn) ms)

data Sentence i = Sentence
  { sentenceContent :: NE.NonEmpty i
  , sentenceEnd :: NE.NonEmpty End
  }
  deriving (Eq, Show, Functor)

data End
  = Exclamation
  | Question
  | Period
  deriving (Eq, Show, Enum, Bounded)

data QoutedSentences i = QoutedSentences
  { qoutedType :: Qoute
  , qoutedSentences :: Sentences i
  }
  deriving (Eq, Show)

data Qoute
  = DoubleQoute
  | Parenthesis
  | Emph
  | Strong
  deriving (Eq, Show, Enum, Bounded)

data Inline i
  = Word Text.Text
  | Reference Text.Text
  | Comma
  | Colon
  | SemiColon
  | Verbatim Text.Text
  | Number Text.Text
  | Qouted (QoutedSentences i)
  deriving (Eq, Show)

data DocAlgebra s b i a = DocAlgebra
  { onSections :: s -> a
  , onBlocks :: b -> a
  , onInlines :: i -> a
  }

countSentencesInInline :: Inline' -> Int
countSentencesInInline (Inline' inline) = case inline of
  Qouted q -> countSentencesInQouted countSentencesInInline q
  _ -> 0

countSentences :: (i -> Int) -> Sentences i -> Int
countSentences cnt = \case
  OpenSentence x ->
    1 + sum (fmap cnt x)
  ClosedSentence (Sentence is _) rest ->
    1 + sum (fmap cnt is) + maybe 0 (countSentences cnt) rest


countSentencesInQouted :: (i -> Int) -> QoutedSentences i -> Int
countSentencesInQouted count (QoutedSentences _ s) =
  countSentences count s

newtype ItemTree i = ItemTree (Item (ItemTree i) i)

compressItem :: (b -> Maybe (NE.NonEmpty (Item b i))) -> Item b i -> Maybe (ItemTree i)
compressItem fn Item {..} = ItemTree <$> do
  contents <- case itemContents of
    [c] -> fmap NE.toList . mapM (compressItem fn) =<< fn c
    [] -> pure []
    _ -> Nothing
  pure $ Item
    { itemType = itemType
    , itemTodo = itemTodo
    , itemTitle = itemTitle
    , itemContents = contents
    }

compressItem' :: Item Block' Inline' -> Maybe (ItemTree Inline')
compressItem' = compressItem (\case
  Block' (Items it) -> Just it
  _ -> Nothing
  )


type Doc = Section'

newtype Section' = Section'
  { getSection :: Section Section' Block' Inline' }
  deriving (Eq)

newtype Block' = Block'
  { getBlock :: Block Block' Inline' }
  deriving (Eq)

newtype Inline' = Inline'
  { getInline :: Inline Inline' }
  deriving (Eq)

type Item' = Item Block' Inline'

