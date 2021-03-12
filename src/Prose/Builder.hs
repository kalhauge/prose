{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-|
This module allows building Prose documents by hand
-}
module Prose.Builder where

-- text
import qualified Data.Text as Text
import Text.Show

-- base
import qualified Data.List.NonEmpty as NE

import Prose.Doc

-- | Create a comment
comment :: Text.Text -> Block b i
comment = Comment . Text.lines

-- | Create a simple comment
comment' :: Text.Text -> Block'
comment' = Block' . comment

-- | Create a paragraph
para :: Sentences i -> Block b i
para = Para

-- | Create a paragraph
para' :: SentenceBuilder (Inline Inline') -> Block'
para' = Block' . para . sen'

-- | Itemized List, fails if the list is empty
items' :: [Item Block' Inline'] -> Block'
items' = maybe (error "list is empty") safeItems' . NE.nonEmpty

-- | Itemized List
safeItems' :: NE.NonEmpty Item' -> Block'
safeItems' =  Block' . Items

-- | A List Item
item' ::
  SentenceBuilder (Inline Inline')
  -> [Block']
  -> Item'
item' = Item Minus Nothing . sen'

-- | A List Todo
todo ::
  Bool -- ^ Is it done?
  -> Sentences Inline'
  -> Item'
todo b s = Item Minus (Just b) s []

withItemType :: Item b i -> ItemType -> Item b i
withItemType i it = i { itemType = it }

withContent :: Item b i -> [ b ] -> Item b i
withContent i blks = i { itemContents = blks }

withItems' :: Item' -> [ Item' ] -> Item'
withItems' i itms = i `withContent` [ items' itms ]

data SentenceBuilder i =
  Current (NE.NonEmpty i) [Sentence i]
  | AllClosed (NE.NonEmpty (Sentence i))
  deriving (Eq, Functor)

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

sen' :: SentenceBuilder (Inline Inline') -> Sentences Inline'
sen' sbs = Inline' <$> sen sbs

sb :: [i] -> SentenceBuilder i
sb i = case NE.nonEmpty i of
  Just i' -> Current i' []
  Nothing -> error "list should be non-empty"

(<.) :: SentenceBuilder i -> [i] -> SentenceBuilder i
(<.) = endWith Period

(<!) :: SentenceBuilder i -> [i] -> SentenceBuilder i
(<!) = endWith Exclamation

(<?) :: SentenceBuilder i -> [i] -> SentenceBuilder i
(<?) = endWith Question

endWith :: End -> SentenceBuilder i -> [i] -> SentenceBuilder i
endWith end sbs is = case sbs of
  Current ne rest -> case NE.nonEmpty is of
    Just xs -> Current xs $ Sentence ne (end NE.:| []) : rest
    Nothing -> AllClosed $ Sentence ne (end NE.:| []) NE.:| rest
  AllClosed (Sentence ne ends NE.:| rest) -> case NE.nonEmpty is of
    Just xs -> Current xs $ Sentence ne (end NE.<| ends) : rest
    Nothing -> AllClosed $ Sentence ne (end NE.<| ends) NE.:| rest

comma :: Inline i
comma = Comma

comma' :: Inline'
comma' = Inline' comma

word :: Text.Text -> Inline i
word = Word

word' :: Text.Text -> Inline'
word' = Inline' . word

number :: Text.Text -> Inline i
number = Number

number' :: Text.Text -> Inline'
number' = Inline' . number

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
    showParen (n > app_prec) (showString "smpl " . showsPrec (app_prec + 1) s)
   where app_prec = 10

instance Show Block' where
  showsPrec n (Block' s) = case s of
    Comment txt -> showParen (n > app_prec) $
      showString "comment' " . shows (Text.intercalate "\n" txt)
    Para p -> showParen (n > app_prec) $
      showString "para' " . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder p)
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
        . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder x)
        . showChar ' ' . shows b
      Item Minus (Just t) x b ->
        showString "todo' " . shows t . showChar ' '
        . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder x)
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
     Parenthesis -> "parens' "
     DoubleQoute -> "dqoute' "
     Emph -> "emph' "
     Strong -> "strong' "

showsSentences :: Int -> Sentences Inline' -> ShowS
showsSentences n s = showParen (n > app_prec) $
  showString "sen' " . showsPrec (app_prec + 1) (getInline <$> toSentenceBuilder s)
 where app_prec = 10
