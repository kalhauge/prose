{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
-- |
--
-- Parse a doc from Markdown.
module Prose.Text.Markdown where

-- base
import Data.String
import Data.Foldable
import Data.Semigroup
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

-- text
import qualified Data.Text as Text

-- prettyprinter
import qualified Data.Text.Prettyprint.Doc as S
import qualified Data.Text.Prettyprint.Doc.Render.Text as S

import Prose.Doc

type Serialized = S.Doc ()
type Serializer s b i a = a -> SerializeConfig s b i -> Serialized
type Serial s b i = SerializeConfig s b i -> Serialized

data SerializeConfig s b i = SerializeConfig
  { sInline' :: Serializer s b i i
  , sInlineWithSpace' :: i -> Serializer s b i i
  , sBlock' :: Serializer s b i b
  , sSection' :: Int -> Serializer s b i s
  , sSentenceSeperator :: Serialized
  , sSentenceCounter :: i -> Int
  , sHasOnlyItems :: b -> Bool
  }

instance IsString (SerializeConfig s b i -> Serialized) where
  fromString str = const (fromString str)

sSingleLine :: Serial s b i -> Serial s b i
sSingleLine f cfg = f ( cfg { sSentenceSeperator = S.space })

type SimpleSerializer a = Serializer SimpleSection SimpleBlock SimpleInline a

simpleSerializeConfig :: SerializeConfig SimpleSection SimpleBlock SimpleInline
simpleSerializeConfig = SerializeConfig
  { sInline' = sInline . getInline
  , sInlineWithSpace' = \i i2 -> sInlineWithSpace (getInline i) (getInline i2)
  , sBlock' = sBlock . getBlock
  , sSection' = \n -> sSection n . getSection
  , sSentenceSeperator = S.hardline
  , sSentenceCounter = countSentencesInSimpleInline
  , sHasOnlyItems = let only = onlyItems only . getBlock in only
  }

serialize :: Serialized -> Text.Text
serialize = S.renderStrict . S.layoutPretty S.defaultLayoutOptions

sI :: Serializer s b i i
sI i cfg = sInline' cfg i cfg

sIWS :: i -> Serializer s b i i
sIWS i1 i2 cfg = sInlineWithSpace' cfg i1 i2 cfg

sB :: Serializer s b i b
sB i cfg = sBlock' cfg i cfg

sS :: Int -> Serializer s b i s
sS n i cfg = sSection' cfg n i cfg

-- Inline
sInline :: Serializer s b i (Inline i)
sInline i cfg = case i of
  Word x -> S.pretty x
  Number x -> S.pretty x
  -- Broken xs -> fold . L.intersperse "-" . map S.pretty . NE.toList $ xs
  Comma -> ","
  Colon -> ":"
  SemiColon -> ";"
  Verbatim txt -> "`" <> S.pretty txt <> "`"
  Qouted s -> sQoutedSentences s cfg

sInlineWithSpace :: Inline i -> Serializer s b i (Inline i)
sInlineWithSpace _ i2 cfg = spaces <> sInline i2 cfg
 where
  spaces = case i2 of
    Word _ -> S.space
    Number _ -> S.space
    -- Broken _ -> S.space
    Verbatim _ -> S.space
    Qouted q
      | countSentencesInQouted (sSentenceCounter cfg) q > 1 ->
        sSentenceSeperator cfg
      | otherwise ->
        S.space
    _ -> mempty

sEnd :: Serializer s b i End
sEnd = \case
  Exclamation -> "!"
  Question -> "?"
  Period -> "."

sInlines :: Serializer s b i [i]
sInlines content = case content of
  c:rest ->
    sI c <> foldMap (uncurry sIWS) (zip content rest)
  [] -> mempty

sSentence :: Serializer s b i (Sentence i)
sSentence (Sentence contents ends) =
  sInlines (NE.toList contents) <> foldMap sEnd ends

sSentences :: Serializer s b i (Sentences i)
sSentences (Sentences sens rest) =
  fold
    . L.intersperse sSentenceSeperator
    $ map sSentence sens ++ case rest of
        [] -> []
        a -> [sInlines a]

sQoutedSentences :: Serializer s b i (QoutedSentences i)
sQoutedSentences (QoutedSentences qoute sentences) = quoted
 where
  quoted =
    case qoute of
    -- SingleQoute -> "'" <> sSentences sentences <> "'"
    DoubleQoute -> "\"" <> sSentences sentences <> "\""
    Parenthesis -> "(" <> sSentences sentences <> ")"
    Emph -> "/" <> sSentences sentences <> "/"
    Strong -> "*" <> sSentences sentences <> "*"


sBlock :: Serializer s b i (Block b i)
sBlock = \case
  Para sb ->
    sSentences sb
    <> const S.hardline

  Comment comments ->
    const $
      S.vsep (map (\txt -> "--" <> S.pretty txt) comments)
      <> S.hardline

  Items items -> \cfg ->
    if all (all (sHasOnlyItems cfg) . itemContents) items
    then compressed items cfg <> S.hardline
    else S.vsep [ sItem s cfg | s <- NE.toList items ]

 where
  compressed items cfg =
    S.vsep [ sCompressedItem s cfg | s <- NE.toList items ]

  sItem (Item tpe _ sens blocks) cfg = S.hang 2 . fold $
    [ sign tpe
    , S.space
    , sSentences sens cfg
    , case blocks of
        [] -> S.hardline
        _ -> S.hardline <> S.hardline <> sBlocks blocks cfg
    ]

  sCompressedItem (Item tpe _ sens blocks) cfg = S.hang 2 . fold $
    [ sign tpe
    , S.space
    , sSentences sens cfg
    , case blocks of
        [] -> mempty
        _ -> S.hardline <> S.hardline <> sBlocks blocks cfg
    ]

  sign = \case
    Minus -> "-"
    Plus -> "+"
    Times -> "*"


onlyItems :: (b -> Bool) -> Block b i -> Bool
onlyItems rec = \case
  Items items ->
    all (all rec . itemContents) items
  _ -> False

sBlocks :: Serializer s b i [b]
sBlocks b cfg =
  S.vsep (($cfg) <$> map sB b)

sSection :: Int -> Serializer s b i (Section s b i)
sSection n sec cfg = S.vsep $
  [ stimes n "#" S.<+> sSingleLine (sSentences $ sectionTitle sec) cfg
  , mempty
  ]
  ++
  [ sBlocks (sectionContent sec) cfg
  | not (L.null (sectionContent sec))
  ]
  ++
  [ sS (n + 1) s cfg
  | s <- sectionSubs sec
  ]

sDoc :: SimpleSerializer SimpleSection
sDoc = sS 1

