{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
This module allows building Prose documents by hand
-}
module Prose.Builder where

-- text
import qualified Data.Text as Text

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
  SentenceBuilder Inline'
  -> [Block']
  -> Item'
item' = Item Minus Nothing . sen

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

colon' :: Inline'
colon' = Inline' Colon

semicolon' :: Inline'
semicolon' = Inline' SemiColon

word :: Text.Text -> Inline i
word = Word

word' :: Text.Text -> Inline'
word' = Inline' . word

number :: Text.Text -> Inline i
number = Number

number' :: Text.Text -> Inline'
number' = Inline' . number

