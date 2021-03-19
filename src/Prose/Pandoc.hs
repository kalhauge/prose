{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
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
import Prose.Recursion
import Prose.Simple

import Text.Pandoc.Builder qualified as PD
-- import Text.Pandoc.Definition

data PandocRes

instance DocR PandocRes where
  type Sec PandocRes = Int -> PD.Blocks
  type Blk PandocRes = PD.Blocks
  type Inl PandocRes = (Bool, PD.Inlines)
  type Sen PandocRes = SenValue PD.Inlines

toPandoc :: Unfix PandocRes ~:> PandocRes
toPandoc = DocMap {..}
 where
  overSec Section {..} n = fold
   [ PD.header n (overSentences sectionTitle)
   , fold sectionContent
   , foldMap ($ n+1) sectionSubs
   ]

  overBlk = \case
    Para p ->
      PD.para $ overSentences p
    Items i ->
      PD.bulletList . map overItem $ NE.toList i
    OrderedItems _ its ->
      PD.orderedList . map overOrderedItem $ NE.toList its
    Comment _ -> mempty

  overSen :: Sen (Unfix PandocRes) b -> Sen PandocRes b
  overSen = SenValue . \case
    OpenSentence ss -> overInlines ss
    ClosedSentence ss ends -> overInlines ss <> foldMap overEnd ends

  overEnd :: End -> PD.Inlines
  overEnd = \case
    Exclamation -> "!"
    Period -> "."
    Question -> "?"

  overInlines (s NE.:| ss) =
    snd s <> flip foldMap ss \(b, y) -> (if b then PD.space else mempty) <> y

  overInl  = \case
    Mark x -> 
      ( False
      , case x of 
        Comma -> PD.str ","
        SemiColon -> PD.str ";"
        Colon -> PD.str ":"
      )
    Verbatim a -> (True, PD.code a)
    Number a -> (True, PD.str a)
    Reference a -> (True, PD.str ("@" <> a))
    Word i -> (True, PD.str i)
    Qouted q -> (True, overQoute q)

  overQoute (QoutedSentences x b) = overSentences b & case x of
    DoubleQoute -> PD.doubleQuoted
    Emph -> PD.emph
    Strong -> PD.strong
    Parenthesis -> \a -> "(" <> a <> ")"
    Brackets -> \a -> "[" <> a <> "]"

  overItem (Item _ _ t bs) =
    PD.plain (overSentences t)
    <> fold bs

  overOrderedItem (OrderedItem _ t bs) =
    PD.plain (overSentences t)
    <> fold bs

  overSentences = \case
    OpenSentences s -> 
      unSenValue s
    ClosedSentences s rst -> 
      unSenValue s <> PD.space <> foldMap overSentences rst

-- | Convert a Simple Doc to a PandocRes
simpleToPandoc :: Simple ~:> PandocRes
simpleToPandoc = cata toPandoc

toPandocDoc :: Section' -> PD.Pandoc
toPandocDoc s = PD.doc $ overSec simpleToPandoc s 1


