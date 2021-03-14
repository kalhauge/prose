{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Prose.Doc where

-- base
import Text.Show
import Prelude hiding (Word)
import Data.List.NonEmpty qualified as NE

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
  | OrderedItems OrderType (NE.NonEmpty (OrderedItem b i))
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

data OrderedItem b i = OrderedItem
  { orderedItemTodo :: Maybe Bool
  , orderedItemTitle :: Sentences i
  , orderedItemContents :: [b]
  }
  deriving (Eq, Show)

data OrderType
  = Numeral
  deriving (Eq, Show, Enum, Bounded)

data Sentences i
  = OpenSentence (NE.NonEmpty i)
  | ClosedSentence
      (Sentence i)
      (Maybe (Sentences i))
  deriving (Eq)

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
  | Brackets
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

sen :: SentenceBuilder i -> Sentences i
sen = \case
  Current ne lst ->
    case NE.nonEmpty lst of
      Just x -> go (Just $ OpenSentence ne) x
      Nothing -> OpenSentence ne
  AllClosed rest ->
    go Nothing rest
 where
  go :: Maybe (Sentences i) -> NE.NonEmpty (Sentence i) -> Sentences i
  go x (s NE.:| ss) = case NE.nonEmpty ss of
    Nothing -> ClosedSentence s x
    Just rst-> go (Just $ ClosedSentence s x) rst

toSentenceBuilder :: Sentences i -> SentenceBuilder i
toSentenceBuilder = go []
 where
  go ss = \case
    OpenSentence s -> Current s ss
    ClosedSentence s r -> case r of
      Just r' -> go (s:ss) r'
      Nothing -> AllClosed (s NE.:| ss)

instance Show i => Show (Sentences i) where
  showsPrec _ s =
    showString "sen" . showsPrec 11 (toSentenceBuilder s)

data SentenceBuilder i =
  Current (NE.NonEmpty i) [Sentence i]
  | AllClosed (NE.NonEmpty (Sentence i))
  deriving (Eq, Functor)



instance Show i => Show (SentenceBuilder i) where
  showsPrec n = showParen (n > app_prec) . \case
    AllClosed sens -> handleRest sens []
    Current i lst ->
      case NE.nonEmpty lst of
        Nothing -> showString "sb " . shows (NE.toList i)
        Just sens -> handleRest sens (NE.toList i)

   where
    handleRest :: Show i => NE.NonEmpty (Sentence i) -> [i] -> ShowS
    handleRest (Sentence i (e NE.:| ends)  NE.:| ns) m =
      ( case NE.nonEmpty ends of
        Just moreEnds -> shows (AllClosed (Sentence i moreEnds NE.:| ns))
        Nothing -> shows (Current i ns)
      )
      . showsEnd e
      . shows m

    app_prec = 10

    -- go (Sentence s (e NE.:| _) NE.:| rest)= case NE.nonEmpty rest of
    --  Nothing -> showString "sb " . shows (NE.toList s) . showsEnd e
    --  Just rest' -> go rest' . shows (NE.toList s) . showsEnd e

    showsEnd :: End -> ShowS
    showsEnd = \case
     Exclamation -> showString " <! "
     Question -> showString " <? "
     Period -> showString " <. "

instance Show Section' where
  showsPrec n (Section' s) =
    showParen (n > app_prec) (showString "Section' " . showsPrec (app_prec + 1) s)
   where app_prec = 10

instance Show Block' where
  showsPrec n (Block' s) = case s of
    Comment txt -> showParen (n > app_prec) $
      showString "comment' " . shows (Text.intercalate "\n" txt)
    Para p -> showParen (n > app_prec) $
      showString "para' " . showsPrec (app_prec + 1) (toSentenceBuilder p)
    Items it -> showParen (n > app_prec) $
      showString "items' " . showListWith showsItem (NE.toList it)
    OrderedItems ot it -> showParen (n > app_prec) $
      showString "ordered' " . shows ot
      . showChar ' ' . showListWith showsOrderedItem (NE.toList it)
   where
    app_prec = 10
    showsItem = \case
      Item Minus Nothing x b ->
        showString "item' "
        . showsPrec (app_prec + 1) (toSentenceBuilder x)
        . showChar ' ' . shows b
      Item Minus (Just t) x b ->
        showString "todo' " . shows t . showChar ' '
        . showsPrec (app_prec + 1) (toSentenceBuilder x)
        . showChar ' ' . shows b
      i -> shows i
    showsOrderedItem (OrderedItem t x b) = case t of
      Nothing ->
        showString "oitem' "
        . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder x)
        . showChar ' ' . shows b
      Just t' ->
        showString "otodo' " . shows t' . showChar ' '
        . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder x)
        . showChar ' ' . shows b


instance Show Inline' where
  showsPrec n (Inline' s) = case s of
    Comma -> showString "comma'"
    Colon -> showString "colon'"
    SemiColon -> showString "semicolon'"
    Word w -> showParen (n > app_prec) $
        showString "word' " . showsPrec (app_prec + 1) w
    Verbatim w -> showParen (n > app_prec) $
        showString "verbatim' " . showsPrec (app_prec + 1) w
    Number w -> showParen (n > app_prec) $
        showString "number' " . showsPrec (app_prec + 1) w
    Reference r -> showParen (n > app_prec) $
        showString "ref' " . showsPrec (app_prec + 1) r
    Qouted w ->
      showsQoutedSentences n w
   where app_prec = 10

-- | Shows an inline with an Inline in.
showsInline :: Int -> Inline Inline' -> ShowS
showsInline n = \case
  Qouted w -> showsQoutedSentences n w
  a -> showsPrec n a

showsQoutedSentences :: Int -> QoutedSentences Inline' -> ShowS
showsQoutedSentences n (QoutedSentences qute x) =
  showParen (n > app_prec) $ showString str . showsPrec (app_prec + 1) (toSentenceBuilder x)
 where
   app_prec = 10
   str = case qute of
     Brackets -> "brackets' "
     Parenthesis -> "parens' "
     DoubleQoute -> "dqoute' "
     Emph -> "emph' "
     Strong -> "strong' "

showsSentences :: Int -> Sentences Inline' -> ShowS
showsSentences n s = showParen (n > app_prec) $
  showString "sen' " . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder s)
 where app_prec = 10

