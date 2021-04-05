{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Prose.Text.Serializer where

-- mtl
import Control.Monad.Reader

-- base
import Data.Foldable
import Control.Category ((.))
import Prelude hiding ((.))
import Data.Functor.Contravariant
import Data.List.NonEmpty qualified as NE
import Data.Semigroup
import Data.String

-- text
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder

import Prose.Doc
import Prose.Simple
import Prose.Recursion

type Builder = Builder.Builder

data SerialConfig = SerialConfig
  { sCfgIndent :: !Int
  , sCfgSingleLine :: !Bool
  , sCfgHeaderDepth :: !Int
  }

data SerializeHandler e = SerializeHandler
  { sCfgInlineSep :: Inl e -> Inl e -> Serialized
  , sCfgBlockSep :: Blk e -> Blk e -> Serialized
  }

newtype Serialized = Serialized 
  { serializeWithConfig :: SerialConfig -> Builder 
  }

instance IsString Serialized where
  fromString s = Serialized (\_ -> Builder.fromString s)

instance Semigroup Serialized where
  Serialized s1 <> Serialized s2 = Serialized (s1 <> s2)

instance Monoid Serialized where
  mempty = Serialized mempty

singleline :: Serialized -> Serialized
singleline (Serialized ser) = Serialized \cfg -> 
  ser (cfg { sCfgSingleLine = True})

indent :: Serialized -> Serialized
indent (Serialized ser) = Serialized \cfg -> 
  ser (cfg { sCfgIndent = sCfgIndent cfg + 2})

increaseHeader :: Serialized -> Serialized
increaseHeader (Serialized ser) = Serialized \cfg -> 
  ser (cfg { sCfgHeaderDepth = sCfgHeaderDepth cfg + 1 })

data Serial

instance DocR Serial where
  type Sec Serial = Int -> Serialized
  type Blk Serial = Serialized
  type Inl Serial = Serialized
  type OpenSen Serial = Serialized
  type ClosedSen Serial = Serialized

serializeSimple :: DocAlgebra Simple Text.Text
serializeSimple = f <$> ser x
 where 
  x = foldR projectR ser
  ser = serializeX simpleHandler
  f a = LazyText.toStrict 
    . Builder.toLazyText 
    $ serializeWithConfig a (SerialConfig 0 False 1)

serializeSimpleR :: Extractor Simple Text.Text
serializeSimpleR =
  serialized simpleHandler

simpleHandler :: SerializeHandler Simple
simpleHandler = SerializeHandler {..}
 where
  sCfgInlineSep _ (Inline' i) = case i of
    Mark _ -> 
      mempty
    Qouted p
      | fromQoutedSentences (cataA countClosedSentences) p > 0 
        && fromQoutedSentences (cataA countSentences) p > 1
        ->
          sSentenceSep 
      | True ->
          " " -- sText (Text.pack (show p))
    _ -> " "

  sCfgBlockSep (Block' e) (Block' e') = case (e, e') of
    (Items _, Items _) -> sEndLine <> sEndLine
    (OrderedItems x1 _, OrderedItems x2 _)
      | x1 == x2 -> sEndLine <> sEndLine
    _ -> sEndLine


serialized :: ProjectableR e => 
  SerializeHandler e 
  -> Extractor e Text.Text
serialized handler = mapV f . serialize handler 
  where 
    f x = LazyText.toStrict . Builder.toLazyText $ 
      serializeWithConfig x (SerialConfig 0 False 1)

serialize :: ProjectableR e 
  => SerializeHandler e 
  -> Extractor e Serialized
serialize handler = foldR projectR (serializeX handler)

sEndLine :: Serialized
sEndLine = "\n"

sText :: Text.Text -> Serialized
sText txt = Serialized \_ -> Builder.fromText txt

sIndent :: Serialized
sIndent = Serialized \cfg -> 
  stimesMonoid (sCfgIndent cfg) " "

sSentenceSep :: Serialized
sSentenceSep = Serialized \cfg -> 
  if sCfgSingleLine cfg 
  then " "
  else
    let Serialized x = sEndLine <> sIndent in x cfg

sHeader :: Serialized 
sHeader = Serialized \(sCfgHeaderDepth -> n) ->
  stimes n "#"

sEscaped :: Text.Text -> Serialized
sEscaped txt = Serialized \_ ->
  Builder.fromText $ Text.intercalate "\\." (Text.splitOn "." txt)

sEscapedEnd :: Text.Text -> Serialized
sEscapedEnd txt = Serialized \_ ->
  let normal = Text.dropWhileEnd (== '.') txt
  in Builder.fromText normal <>
    stimesMonoid (Text.length txt - Text.length normal) "\\."

serializeX :: forall e.
    ProjectableR e
 => SerializeHandler e 
 -> Extractor e Serialized
 -> DocAlgebra e Serialized
serializeX SerializeHandler {..} ex = DocAlgebra {..}
 where
  fromSection Section {..} = 
    sHeader <> " " <> singleline (fromSentences sectionTitle) <> sEndLine
    <> (case NE.nonEmpty sectionContent of
      Just blks -> sEndLine <> sIntercalate sCfgBlockSep (overBlk ex) blks
      Nothing -> mempty
    )
    <> foldMap 
        (\s -> sEndLine <> increaseHeader (overSec ex s)) 
        sectionSubs

  fromSentences = \case
    OpenSentences sen ->
      overOpenSen ex sen
    ClosedSentences s rest -> 
      overClosedSen ex s 
      <> foldMap (\m -> sSentenceSep <> fromSentences m) rest

  sIntercalate :: 
       (a -> a -> Serialized) 
    -> (a -> Serialized) 
    -> NE.NonEmpty a 
    -> Serialized
  sIntercalate sep x (r NE.:| rest) = 
    x r <> foldMap
      (\(r1, r2) -> sep r1 r2 <> x r2)
      (zip (r:rest) rest)

  fromBlocks = 
    sIntercalate sCfgBlockSep (overBlk ex) 

  fromInline = \case
    Word x -> sEscaped x
    Number x -> sEscapedEnd x
    Mark Comma -> ","
    Mark Colon -> ":"
    Mark SemiColon -> ";"
    Verbatim txt -> "`" <> sText txt <> "`"
    Reference txt -> "@" <> sText txt
    Qouted q -> fromQoutedSentences q

  fromBlock = \case
    Para sb ->
      sIndent <> fromSentences sb <> sEndLine

    Comment its ->
      foldMap
        (\i -> sIndent <> "--" <> sText i <> sEndLine)
        its

    Items itms -> 
      case traverse compressItem itms of
        Just trees ->
          foldMap sItemTree trees
        Nothing ->
          let (i NE.:| its) = itms
          in fromItem i <> foldMap (\i' -> sEndLine <> fromItem i') its

    OrderedItems _n (i NE.:| its) ->
      fromNumberedItem 1 i
      <> foldMap
        (\(n, i') -> sEndLine <> fromNumberedItem n i')
        (zip [2..] its)

    CodeBlock n txt ->
      sIndent <> "```" <> foldMap sText n <> sEndLine
      <> foldMap (\a -> sIndent <> sText a <> sEndLine) txt
      <> sIndent <> "```" <> sEndLine

  sItemTree (ItemTree it _ tt blks) = sIndent <> indent 
    ( fromItemType it
     <> fromSentences tt
     <> sEndLine
     <> case NE.nonEmpty blks of
          Nothing -> mempty
          Just trees -> foldMap sItemTree trees
    )

  fromItem (Item it _ tt blks) = sIndent <> indent 
    ( fromItemType it
     <> fromSentences tt
     <> sEndLine
     <> case NE.nonEmpty blks of
          Nothing -> mempty
          Just blks' ->
            sEndLine <> fromBlocks blks'
    )

  fromItemType = \case
    Minus -> "- "
    Plus  -> "+ "
    Times -> "* "

  fromOrderedItem = 
    fromNumberedItem 0

  fromNumberedItem x (OrderedItem _ tt blks) = sIndent <> 
    indent (
      (sText . Text.pack . show $ (x :: Int))
      <> ") "
      <> fromSentences tt 
      <> sEndLine
      <> ( case NE.nonEmpty blks of
          Nothing -> mempty
          Just blks' ->
            sEndLine <> fromBlocks blks'
      )
    )

  fromQoutedSentences (QoutedSentences qoute sens) =
    let qouteit f t = f <> fromSentences sens <> t
    in case qoute of
      Brackets -> qouteit "[" "]"
      DoubleQoute -> qouteit "\"" "\""
      Parenthesis -> qouteit "(" ")"
      Emph -> qouteit "/" "/"
      Strong -> qouteit "*" "*"

  fromInlines = 
    sIntercalate sCfgInlineSep (overInl ex)

  fromSentence :: Sentence b e -> Serialized
  fromSentence = \case
    ClosedSentence i ends -> 
      fromInlines i <> foldMap fromEnd ends
    OpenSentence i -> 
      fromInlines i

  fromEnd :: End -> Serialized
  fromEnd = \case
    Exclamation -> "!"
    Question -> "?"
    Period -> "."
