{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Prose.Pandoc where

-- base
import Data.Foldable
import Data.Function
import Data.List.NonEmpty qualified as NE

import Prose.Doc

import Text.Pandoc.Builder qualified as PD
-- import Text.Pandoc.Definition

toPandoc :: Section' -> PD.Pandoc
toPandoc s =
  PD.doc (toS 1 s)

 where
  toS n = toPandocSection toS toB toI n . getSection
  toI = toPandocInline toI . getInline
  toB = toPandocBlock toB toI . getBlock

toPandocSection ::
  (Int -> s -> PD.Blocks)
  -> (b -> PD.Blocks)
  -> (i -> (Bool, PD.Inlines))
  -> Int
  -> Section s b i
  -> PD.Blocks
toPandocSection toS toB toI n Section { .. } = fold
  [ PD.header n (toPandocSentences toI sectionTitle)
  , foldMap toB sectionContent
  , foldMap (toS (n+1)) sectionSubs
  ]

toPandocBlock :: (b -> PD.Blocks) -> (i -> (Bool, PD.Inlines)) -> Block b i -> PD.Blocks
toPandocBlock toB toI = \case
  Para p ->
    PD.para (toPandocSentences toI p)
  Items i ->
    PD.bulletList (map toPandocItem $ NE.toList i)
  OrderedItems _ its ->
    PD.orderedList (map toPandocOrderedItem $ NE.toList its)
  Comment _ -> mempty

 where
  toPandocItem (Item _ _ t bs) =
    PD.plain (toPandocSentences toI t)
    <> foldMap toB bs

  toPandocOrderedItem (OrderedItem _ t bs) =
    PD.plain (toPandocSentences toI t)
    <> foldMap toB bs

toPandocSentences :: (i -> (Bool, PD.Inlines)) -> Sentences i -> PD.Inlines
toPandocSentences toI = \case
  OpenSentence ss ->
    toInlines ss
  ClosedSentence (Sentence ss ends) mrest ->
    toInlines ss
    <> foldMap toEnd ends
    <> foldMap (const PD.space <> toPandocSentences toI) mrest

 where
   toEnd = \case
    Exclamation -> "!"
    Period -> "."
    Question -> "?"

   toInlines (s NE.:| ss) =
      snd (toI s) <> flip foldMap ss \x ->
          let (b, y) = toI x in (if b then PD.space else mempty) <> y


toPandocInline :: (i -> (Bool, PD.Inlines)) -> Inline i -> (Bool, PD.Inlines)
toPandocInline toI = \case
  Word i -> (True, PD.str i)
  Comma -> (False, PD.str ",")
  SemiColon -> (False, PD.str ";")
  Colon -> (False, PD.str ":")
  Verbatim a -> (True, PD.code a)
  Number a -> (True, PD.str a)
  Reference a -> (True, PD.str ("@" <> a))
  Qouted q -> (True, toQoute q)
 where
  toQoute (QoutedSentences x b) = toPandocSentences toI b & case x of
    DoubleQoute -> PD.doubleQuoted
    Emph -> PD.emph
    Strong -> PD.strong
    Parenthesis -> \a -> "(" <> a <> ")"
    Brackets -> \a -> "[" <> a <> "]"




