{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Prose.Simple where

-- base
import Control.Category
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import Text.Show
import Prelude hiding (id, (.))

-- lens
import Control.Lens hiding (Simple)

-- text
import Data.Text qualified as Text

import Prose.Doc
import Prose.Recursion

data Simple
instance DocR Simple where
  type Sec Simple = Section'
  type Inl Simple = Inline'
  type Blk Simple = Block'
  type OpenSen Simple = Sentence 'Open Simple
  type ClosedSen Simple = Sentence 'Closed Simple

instance OrdR Simple
instance EqR Simple
instance ShowR Simple

type Doc = Section'



newtype Section' = Section'
  {getSection :: Section Simple}
  deriving (Eq, Ord)

instance Show Section' where
  showsPrec = flip $ overSec showSimple

newtype Block' = Block'
  {getBlock :: Block Simple}
  deriving (Eq, Ord)

instance Show Block' where
  showsPrec = flip (overBlk showSimple)

newtype Inline' = Inline'
  {getInline :: Inline Simple}
  deriving (Eq, Ord)

instance Show Inline' where
  showsPrec = flip (overInl showSimple)



type Item' = Item Simple

instance ProjectableR Simple where
  projectR =
    DocMap $
      Instance
        { onSec = \(Section' sec) -> sec
        , onBlk = \(Block' blk) -> blk
        , onInl = \(Inline' inl) -> inl
        , onOpenSen = id
        , onClosedSen = id
        }

instance EmbedableR Simple where
  embedR =
    DocMap $
      Instance
        { onSec = Section'
        , onBlk = Block'
        , onInl = Inline'
        , onOpenSen = id
        , onClosedSen = id
        }


showSimple :: Extractor Simple (Int -> ShowS)
showSimple = extractR showDoc

countSimpleSentences :: Extractor Simple Int
countSimpleSentences =
  mapV getSum . extractR countSentences

countSimpleWords :: Extractor Simple Int
countSimpleWords =
  mapV getSum . extractR countWords

-- instance Show Section' where
--   showsPrec n (Section' s) =
--     showParen (n > app_prec) (showString "Section' " . showsPrec (app_prec + 1) s)
--    where app_prec = 10

-- | A default fold, should be overloaded
showDoc :: DocAlgebra (Value (Int -> ShowS)) (Int -> ShowS)
showDoc = DocAlgebra{..}
 where
  fromSection Section{..} n =
    showParen (n > app_prec) $
      showString "sec' "
        . fromSentences _sectionTitle (app_prec + 1)
        . showChar ' '
        . showListWith ($ 0) _sectionContent
        . showChar ' '
        . showListWith ($ 0) _sectionSubs

  fromBlock blk n = case blk of
    Para s ->
      showParen (n > app_prec) $
        showString "para' "
          . fromSentences s (app_prec + 1)
    Comment x ->
      showParen (n > app_prec) $
        showString "comment' "
          . shows (Text.intercalate "\n" x)
    CodeBlock nm x ->
      showParen (n > app_prec) $
        showString "codeblock' "
          . shows nm
          . showChar ' '
          . shows x
    Items its ->
      showParen (n > app_prec) $
        showString "items' "
          . showListWith (`fromItem` n) (NE.toList its)
    OrderedItems x its ->
      showParen (n > app_prec) $
        showString "ordered' "
          . shows x
          . showChar ' '
          . showListWith (`fromOrderedItem` n) (NE.toList its)

  fromItem Item{..} n = 
    showParen (n > app_prec) $
      showString "Item"
      . showChar ' '
      . showsPrec app_prec _itemType
      . showChar ' '
      . showsPrec app_prec _itemTodo
      . showChar ' '
      . fromSentences _itemTitle app_prec
      . showChar ' '
      . showListWith ($ 0) _itemContents
  -- fromSentences itemTitle
  -- <> fold itemContents

  fromOrderedItem OrderedItem{} = const $ shows ()
  -- fromSentences orderedItemTitle
  -- <> fold orderedItemContents

  fromInline i n = showParen (n > app_prec) $ case i of
    Word w -> showString "word' " . shows w
    Reference w -> showString "ref' " . shows w
    Verbatim w -> showString "verb' " . shows w
    Number w -> showString "num' " . shows w
    Mark m -> showString "mark' " . shows m
    Emdash -> showString "emdash'"
    Qouted q -> fromQoutedSentences q n

  fromQoutedSentences (QoutedSentences qute sen) n =
    showParen (n > app_prec) $
      showString str
        . fromSentences sen (app_prec + 1)
   where
    str = case qute of
      Brackets -> "brackets' "
      Parenthesis -> "parens' "
      DoubleQoute -> "dqoute' "
      Emph -> "emph' "
      Strong -> "strong' "

  fromSentences sens n = case sens of
    OpenSentences sen' ->
      showParen (n > app_prec) $
        showString "OpenSentences "
          . sen' n
    ClosedSentences sen' rest ->
      showParen (n > app_prec) $
        showString "ClosedSentences "
          . sen' n
          . showChar ' '
          . maybe (const id) fromSentences rest n

  fromSentence :: forall b. Sentence b (Value (Int -> ShowS)) -> Int -> ShowS
  fromSentence sen n = case sen of
    OpenSentence inlines' ->
      showParen (n > app_prec) $
        showString "open' "
          . showListWith ($ 0) (NE.toList inlines')
    ClosedSentence inlines' end ->
      showParen (n > app_prec) $
        showString "closed' "
          . showListWith (\(x :: Int -> ShowS) -> x 0) (NE.toList inlines')
          . showChar ' '
          . showsPrec (app_prec + 1) (NE.toList end)

  app_prec = 10

makeWrapped ''Section'
makeWrapped ''Block'
makeWrapped ''Inline'

instance LensR Simple where
  lSec = _Wrapped
  lBlk = _Wrapped
  lInl = _Wrapped
  lOpenSen = iso id id
  lClosedSen = iso id id

instance CoLensR Simple where
  colSec = _Unwrapped
  colBlk = _Unwrapped
  colInl = _Unwrapped
  colOpenSen = iso id id
  colClosedSen = iso id id

