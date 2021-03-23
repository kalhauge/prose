{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
module Prose.Text.Serializer where

-- mtl
import Control.Monad.Reader

-- base
import Data.Foldable
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
  { sCfgIndent :: Int
  , sCfgSingleLine :: Bool
  }

data SerializeHandler e = SerializeHandler
  { sCfgInlineSep :: Inl e -> Inl e -> Builder
  , sCfgBlockSep :: Blk e -> Blk e -> Builder
  -- , sCfgCompressItem :: Item b i -> Maybe (ItemTree i)
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
  ser (cfg { sCfgSingleLine = True })

data Serial

instance DocR Serial where
  type Sec Serial = Int -> Serialized
  type Blk Serial = Serialized
  type Inl Serial = Serialized
  type Sen Serial = SenValue Serialized

serialized :: ProjectableR e => SerializeHandler e -> Extractor e Text.Text
serialized handler = contramapExtractor (serialize handler) $ Extractor {..}
  where 
    fromSec :: (Int -> Serialized) -> Text.Text
    fromSec sn = LazyText.toStrict . Builder.toLazyText $ 
      serializeWithConfig (sn 0) (SerialConfig 0 True)



serialize :: ProjectableR e => SerializeHandler e -> e ~:> Serial
serialize handler = cata (serializeR handler)

serializeR :: forall a e. 
  a ~ Serial
  => SerializeHandler e
  -> Unfix a ~:> a
serializeR SerializeHandler {..} = DocMap {..}
 where
  overSec Section {..} n =
    stimes n "#" <> " " <> singleline (sSentences sectionTitle) <> sEndLine
    <> (case NE.nonEmpty sectionContent of
      Just blks -> sEndLine <> fold blks
      Nothing -> mempty
    )
    <> foldMap (\s -> sEndLine <> s (n+1)) sectionSubs

  sSentences = \case
    OpenSentences sen ->
      unSenValue sen
    ClosedSentences s rest -> 
      unSenValue s <> foldMap (\m -> sSentenceSep <> sSentences m) rest

  sSentenceSep = Serialized \cfg -> 
    if sCfgSingleLine cfg 
    then " "
    else let Serialized x = sEndLine <> sIndent in x cfg

  sIndent = Serialized \cfg -> 
    stimesMonoid (sCfgIndent cfg) " "

  sEndLine :: Serialized
  sEndLine = "\n"


-- data SerialConfig b i = SerialConfig
--   { sCfgInline :: Serializer b i i
--   , sCfgBlock :: Serializer b i b
--   , sCfgIndent :: Int
--   , sCfgSingleLine :: Bool
--   , sCfgInlineSep :: Serializer b i (i, i)
--   , sCfgBlockSep :: Serializer b i (b, b)
--   , sCfgCompressItem :: Item b i -> Maybe (ItemTree i)
--   }
-- 
-- 
-- newtype Serializer a = Serial
--   { runSerializer :: a -> Builder
--   }
-- 
-- instance Contravariant Serializer where
--   {-# INLINE contramap #-}
--   contramap f s = Serial \cfg a -> runSerializer s cfg (f a)
-- 



-- serializeWith :: SerialConfig b i -> Serializer b i a -> a -> Text.Text
-- serializeWith cfg s =
--   LazyText.toStrict
--   . Builder.toLazyText
--   . runSerializer s cfg
-- 
-- over :: (a -> Serializer b i ()) -> Serializer b i a
-- over fn = Serial \cfg a -> runSerializer (fn a) cfg ()
-- 
-- overF :: Foldable f => (a -> f (Serializer b i ())) -> Serializer b i a
-- overF fn = Serial \cfg -> foldMap (\a -> runSerializer a cfg ()) . fn
-- 
-- indent :: Serializer b i a -> Serializer b i a
-- indent s = Serial \cfg -> runSerializer s (cfg { sCfgIndent = sCfgIndent cfg + 2 })
-- 
-- sText :: Serializer b i Text.Text
-- sText = Serial \_ txt -> Builder.fromText txt
-- 
-- sEscaped :: Serializer b i Text.Text
-- sEscaped = Serial \_ txt ->
--   Builder.fromText $ Text.intercalate "\\." (Text.splitOn "." txt)
-- 
-- sEscapedEnd :: Serializer b i Text.Text
-- sEscapedEnd = Serial \_ txt ->
--   let normal = Text.dropWhileEnd (== '.') txt
--   in Builder.fromText normal <>
--     stimesMonoid (Text.length txt - Text.length normal) "\\."
-- 
-- sWithCompressedItems :: (Maybe (NE.NonEmpty (ItemTree i)) -> Serializer b i ())
--   -> Serializer b i (NE.NonEmpty (Item b i))
-- sWithCompressedItems s = Serial \cfg a ->
--   runSerializer (s $ mapM (sCfgCompressItem cfg) a) cfg ()
-- 
-- 
-- 
-- sNewLine :: Serializer b i a
-- sNewLine = sIndent <> "\n"
-- 
-- 
-- sInline :: Serializer b i (Inline i)
-- sInline = over \case
--   Word x -> x >$ sEscaped
--   Number x -> x >$ sEscapedEnd
--   Comma -> ","
--   Colon -> ":"
--   SemiColon -> ";"
--   Verbatim txt -> "`" <> (txt >$ sText) <> "`"
--   Reference txt -> "@" <> (txt >$ sText)
--   Qouted q -> q >$ sQoutedSentences
-- 
-- sInlineSep :: Serializer b i (i, i)
-- sInlineSep = Serial (\cfg -> runSerializer (sCfgInlineSep cfg) cfg)
-- 
-- sBlockSep :: Serializer b i (b, b)
-- sBlockSep = Serial (\cfg -> runSerializer (sCfgBlockSep cfg) cfg)
-- 
-- sI :: Serializer b i i
-- sI = Serial (\cfg i -> runSerializer (sCfgInline cfg) cfg i)
-- 
-- sB :: Serializer b i b
-- sB = Serial (\cfg b -> runSerializer (sCfgBlock cfg) cfg b)
-- 
-- sBlocks :: Serializer b i (NE.NonEmpty b)
-- sBlocks = over \(r NE.:| rest) ->
--   (sB $< r)
--   <> foldMap
--     (\p@(_, r2) -> (sBlockSep $< p) <> (sB $< r2))
--     (zip (r:rest) rest)
-- 
-- sBlock :: Serializer b i (Block b i)
-- sBlock = over \case
-- 
--   Para sb ->
--     sIndent <> (sSentences $< sb) <> sEndLine
-- 
--   Comment its ->
--     foldMap
--       (\i -> sIndent <> "--" <> (sText $< i) <> sEndLine)
--       its
-- 
--   Items itms -> itms >$ sWithCompressedItems \case
--     Just trees ->
--       foldMap (sItemTree $<) trees
--     Nothing ->
--       let (i NE.:| its) = itms
--       in (sItem $< i) <> foldMap (\i' -> sEndLine <> (sItem $< i')) its
-- 
--   OrderedItems _n itms ->
--       let (i NE.:| its) = itms
--       in
--         (sNumberedItem 1 $< i)
--         <> foldMap
--           (\(n, i') -> sEndLine <> (sNumberedItem n $< i'))
--           (zip [2..] its)
-- 
--  where
--   sItemTree :: Serializer b i (ItemTree i)
--   sItemTree = sIndent <> over \(ItemTree (Item it _ tt blks)) -> indent $
--      (sItemType $< it)
--      <> (sSentences $< tt)
--      <> sEndLine
--      <> case NE.nonEmpty blks of
--           Nothing -> mempty
--           Just trees -> foldMap (sItemTree $<) trees
-- 
--   sItem = sIndent <> over \(Item it _ tt blks) -> indent $
--      (sItemType $< it)
--      <> (sSentences $< tt)
--      <> sEndLine
--      <> case NE.nonEmpty blks of
--           Nothing -> mempty
--           Just blks' ->
--             sEndLine <> (sBlocks $< blks')
-- 
--   sItemType = over \case
--     Minus -> "- "
--     Plus  -> "+ "
--     Times -> "* "
-- 
--   sNumberedItem x = sIndent <> over \(OrderedItem _ tt blks) -> indent $
--     (Text.pack (show (x :: Int)) >$ sText) <> ") " <> (sSentences $< tt) <> sEndLine
--      <> case NE.nonEmpty blks of
--           Nothing -> mempty
--           Just blks' ->
--             sEndLine <> (sBlocks $< blks')
-- 
-- sQoutedSentences :: Serializer b i (QoutedSentences i)
-- sQoutedSentences = over \(QoutedSentences qoute sens) ->
--   let qouteit f t = f <> (sens >$ sSentences) <> t
--   in case qoute of
--     Brackets -> qouteit "[" "]"
--     DoubleQoute -> qouteit "\"" "\""
--     Parenthesis -> qouteit "(" ")"
--     Emph -> qouteit "/" "/"
--     Strong -> qouteit "*" "*"
-- 
-- sSentences :: Serializer b i (Sentences i)
-- sSentences = over \case
--   OpenSentence inlines ->
--     sInlines $< inlines
--   ClosedSentence s rest -> case rest of
--     Just m -> (sSentence $< s) <> sSentenceSep <> (sSentences $< m)
--     Nothing -> sSentence $< s
-- 
-- 
-- sInlines' :: Serializer b i [i]
-- sInlines' = NE.nonEmpty >$< over \case
--   Just x -> sInlines $< x
--   Nothing -> mempty
-- 
-- sInlines :: Serializer b i (NE.NonEmpty i)
-- sInlines = over \(r NE.:| rest) ->
--   (sI $< r)
--   <> foldMap
--     (\p@(_, r2) -> (sInlineSep $< p) <> (sI $< r2))
--     (zip (r:rest) rest)
-- 
-- sSentence :: Serializer b i (Sentence i)
-- sSentence = overF \(Sentence i ends) ->
--   [ sInlines $< i
--   , foldMap (sEnd $<) ends
--   ]
-- 
-- sEnd :: Serializer b i End
-- sEnd = over \case
--   Exclamation -> "!"
--   Question -> "?"
--   Period -> "."
-- 
-- 
-- type SimpleSerializer = Serializer Block' Inline'
-- 
-- sSimpleSection :: Int -> SimpleSerializer Section'
-- sSimpleSection n = getSection >$< sSection sSimpleSection n
-- 
-- sDoc :: SimpleSerializer Section'
-- sDoc = sSimpleSection 1
-- 
-- simpleSerializeConfig :: SerialConfig Block' Inline'
-- simpleSerializeConfig = SerialConfig
--   { sCfgInline = getInline >$< sInline
--   , sCfgBlock = getBlock >$< sBlock
--   , sCfgSingleLine = False
--   , sCfgIndent = 0
--   , sCfgInlineSep = over \(_, i) -> case getInline i of
--       Comma -> mempty
--       SemiColon -> mempty
--       Colon -> mempty
--       Qouted p
--         | countSentencesInQouted countSentencesInInline p > 1 -> sSentenceSep
--       _ -> " "
--   , sCfgBlockSep = over \case
--     (Block' (Items _), Block' (Items _)) -> sEndLine <> sEndLine
--     (Block' (OrderedItems x1 _), Block' (OrderedItems x2 _))
--       | x1 == x2 -> sEndLine <> sEndLine
--     _ -> sEndLine
--   , sCfgCompressItem = compressItem'
--   }
-- 
-- runSimpleSerializer :: SimpleSerializer a -> a -> Text.Text
-- runSimpleSerializer = serializeWith simpleSerializeConfig
-- 
-- class Serializable x where
--   serialize :: x -> Text.Text
-- 
-- instance Serializable Inline' where
--   serialize = runSimpleSerializer sI
-- 
-- instance Serializable Block' where
--   serialize = runSimpleSerializer sB
-- 
-- instance Serializable Section' where
--   serialize = runSimpleSerializer sDoc
-- 
-- instance Serializable (Sentences Inline') where
--   serialize = runSimpleSerializer sSentences
-- 
-- 
