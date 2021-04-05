{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Prose.Pandoc where

-- base
import Data.Foldable
import Data.Function
import Data.List.NonEmpty qualified as NE
-- text
import Data.Text qualified as Text
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
  type OpenSen PandocRes = PD.Inlines
  type ClosedSen PandocRes = PD.Inlines

toPandoc :: Unfix PandocRes ~:> PandocRes
toPandoc = DocMap $ Instance {..}
  where
    onSec Section {..} n =
      fold
        [ PD.header n (pandocSentences sectionTitle),
          fold sectionContent,
          foldMap ($ n + 1) sectionSubs
        ]

    onBlk = \case
      Para p ->
        PD.para $ pandocSentences p
      CodeBlock nm p ->
        PD.codeBlockWith ("", toList nm, []) (Text.unlines p)
      Items i ->
        PD.bulletList . map onItem $ NE.toList i
      OrderedItems _ its ->
        PD.orderedList . map onOrderedItem $ NE.toList its
      Comment _ -> mempty

    onOpenSen :: OpenSen (Unfix PandocRes) -> OpenSen PandocRes
    onOpenSen = \case
      OpenSentence ss -> onInlines ss

    onClosedSen :: ClosedSen (Unfix PandocRes) -> ClosedSen PandocRes
    onClosedSen = \case
      ClosedSentence ss ends -> onInlines ss <> foldMap onEnd ends

    onEnd :: End -> PD.Inlines
    onEnd = \case
      Exclamation -> "!"
      Period -> "."
      Question -> "?"

    onInlines (s NE.:| ss) =
      snd s <> flip foldMap ss \(b, y) -> (if b then PD.space else mempty) <> y

    onInl = \case
      Mark x ->
        ( False,
          case x of
            Comma -> PD.str ","
            SemiColon -> PD.str ";"
            Colon -> PD.str ":"
        )
      Verbatim a -> (True, PD.code a)
      Emdash -> (True, PD.str "—")
      Number a -> (True, PD.str a)
      Reference a -> (True, PD.str ("@" <> a))
      Word i -> (True, PD.str i)
      Qouted q -> (True, onQoute q)

    onQoute (QoutedSentences x b) =
      pandocSentences b & case x of
        DoubleQoute -> PD.doubleQuoted
        Emph -> PD.emph
        Strong -> PD.strong
        Parenthesis -> \a -> "(" <> a <> ")"
        Brackets -> \a -> "[" <> a <> "]"

    onItem (Item _ _ t bs) =
      PD.plain (pandocSentences t)
        <> fold bs

    onOrderedItem (OrderedItem _ t bs) =
      PD.plain (pandocSentences t)
        <> fold bs

pandocSentences :: Sentences PandocRes -> PD.Inlines
pandocSentences = \case
  OpenSentences s ->
    s
  ClosedSentences s rst ->
    s <> PD.space <> foldMap pandocSentences rst

-- | Convert a Simple Doc to a PandocRes
simpleToPandoc :: Simple ~:> PandocRes
simpleToPandoc = cata toPandoc

toPandocDoc :: Section' -> PD.Pandoc
toPandocDoc s = PD.doc $ overSec simpleToPandoc s 1
