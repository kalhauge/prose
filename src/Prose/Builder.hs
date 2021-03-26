{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
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
import Prose.Simple
import Data.Maybe


-- | Create a comment
comment :: Text.Text -> Block e
comment = Comment . Text.lines

-- | Create a simple comment
comment' :: Text.Text -> Block'
comment' = Block' . comment

-- | Create a paragraph
para :: Sentences e -> Block e
para = Para

-- | Create a paragraph
para' :: SentenceBuilder Simple -> Block'
para' = Block' . para . fromSentenceBuilder

-- | Itemized List, fails if the list is empty
items' :: [Item Simple] -> Block'
items' = maybe (error "list is empty") safeItems' . NE.nonEmpty

-- | Itemized List
safeItems' :: NE.NonEmpty Item' -> Block'
safeItems' =  Block' . Items

-- | A List Item
item' ::
  SentenceBuilder e
  -> [Blk e]
  -> Item e
item' = Item Minus Nothing . fromSentenceBuilder

-- | A List Todo
todo ::
  Bool -- ^ Is it done?
  -> Sentences e
  -> Item e
todo b s = Item Minus (Just b) s []

sb :: [Inl Simple] -> SentenceBuilder Simple
sb i = case NE.nonEmpty i of
  Just i' -> Current (OpenSentence i') []
  Nothing -> error "list should be non-empty"

open' :: [Inl Simple] -> Sentence 'Open Simple
open' = OpenSentence 
  . fromMaybe (error "list should be nonEmpty") 
  . NE.nonEmpty

closed' :: [Inl Simple] -> [End] -> Sentence 'Closed Simple
closed' inl ends = ClosedSentence 
  ( fromMaybe (error "words should be nonEmpty") 
  $ NE.nonEmpty inl
  )
  ( fromMaybe (error "ends should be nonEmpty") 
  $ NE.nonEmpty ends
  )

mark' :: Mark -> Inl Simple
mark' = Inline' . Mark

-- (<.) :: SentenceBuilder e -> [Inl e] -> SentenceBuilder e
-- (<.) = endWith Period
-- 
-- (<!) :: SentenceBuilder e -> [Inl e] -> SentenceBuilder e
-- (<!) = endWith Exclamation
-- 
-- (<?) :: SentenceBuilder e -> [Inl e] -> SentenceBuilder e
-- (<?) = endWith Question
-- 
-- endWith :: End -> SentenceBuilder e -> [Inl e] -> SentenceBuilder e
-- endWith end sbs is = case sbs of
--   Current ne rest -> case NE.nonEmpty is of
--     Just xs -> 
--       Current (sen $ OpenSentence xs) 
--       $ sen (ClosedSentence ne (end NE.:| []) : rest)
--     Nothing -> 
--       AllClosed 
--       $ ClosedSentence ne (end NE.:| []) NE.:| rest
--   AllClosed (ClosedSentence ne ends NE.:| rest) -> case NE.nonEmpty is of
--     Just xs -> 
--       Current xs 
--       $ Sentence ne (end NE.<| ends) : rest
--     Nothing -> 
--       AllClosed 
--       $ Sentence ne (end NE.<| ends) NE.:| rest

comma' :: Inline'
comma' = Inline' $ Mark Comma

colon' :: Inline'
colon' = Inline' $ Mark Colon

semicolon' :: Inline'
semicolon' = Inline' $ Mark SemiColon

word' :: Text.Text -> Inline'
word' = Inline' . Word

number' :: Text.Text -> Inline'
number' = Inline' . Number

verbatim' :: Text.Text -> Inline'
verbatim' = Inline' . Verbatim

