{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Prose.Text.Serializer where

-- base
import Data.Functor.Contravariant
import Data.List.NonEmpty qualified as NE
import Data.Semigroup
import Data.String

-- text
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder

import Prose.Doc

data SerialConfig b i = SerialConfig
  { sCfgInline :: Serializer b i i
  , sCfgBlock :: Serializer b i b
  , sCfgIndent :: Int
  , sCfgSingleLine :: Bool
  , sCfgInlineSep :: Serializer b i (i, i)
  , sCfgBlockSep :: Serializer b i (b, b)
  , sCfgCompressItem :: Item b i -> Maybe (ItemTree i)
  }

type Builder = Builder.Builder

newtype Serializer b i a = Serial
  { runSerializer :: SerialConfig b i -> a -> Builder
  }

instance Contravariant (Serializer b i) where
  {-# INLINE contramap #-}
  contramap f s = Serial \cfg a -> runSerializer s cfg (f a)

instance IsString (Serializer b i a) where
  fromString s = Serial \_ _ -> Builder.fromString s

instance Semigroup (Serializer b i a) where
  s1 <> s2 = Serial \cfg a ->
    runSerializer s1 cfg a <> runSerializer s2 cfg a

instance Monoid (Serializer b i a) where
  mempty = Serial \_ _ -> mempty

serialize :: SerialConfig b i -> Serializer b i a -> a -> Text.Text
serialize cfg s =
  LazyText.toStrict
  . Builder.toLazyText
  . runSerializer s cfg

over :: (a -> Serializer b i ()) -> Serializer b i a
over fn = Serial \cfg a -> runSerializer (fn a) cfg ()

overF :: Foldable f => (a -> f (Serializer b i ())) -> Serializer b i a
overF fn = Serial \cfg -> foldMap (\a -> runSerializer a cfg ()) . fn

indent :: Serializer b i a -> Serializer b i a
indent s = Serial \cfg -> runSerializer s (cfg { sCfgIndent = sCfgIndent cfg + 2 })

singleline :: Serializer b i a -> Serializer b i a
singleline s = Serial \cfg -> runSerializer s (cfg { sCfgSingleLine = True })

sText :: Serializer b i Text.Text
sText = Serial \_ txt -> Builder.fromText txt

sEscaped :: Serializer b i Text.Text
sEscaped = Serial \_ txt ->
  Builder.fromText $ Text.intercalate "\\." (Text.splitOn "." txt)

sEscapedEnd :: Serializer b i Text.Text
sEscapedEnd = Serial \_ txt ->
  let normal = Text.dropWhileEnd (== '.') txt
  in Builder.fromText normal <>
    stimesMonoid (Text.length txt - Text.length normal) "\\."

sWithCompressedItems :: (Maybe (NE.NonEmpty (ItemTree i)) -> Serializer b i ())
  -> Serializer b i (NE.NonEmpty (Item b i))
sWithCompressedItems s = Serial \cfg a ->
  runSerializer (s $ mapM (sCfgCompressItem cfg) a) cfg ()

sIndent :: Serializer b i a
sIndent = Serial \cfg _ ->
  stimesMonoid (sCfgIndent cfg) " "

sSentenceSep :: Serializer b i ()
sSentenceSep = Serial \cfg _ ->
  if sCfgSingleLine cfg
  then " "
  else "\n" <> runSerializer sIndent cfg ()

sNewLine :: Serializer b i a
sNewLine = sIndent <> "\n"

sEndLine :: Serializer b i ()
sEndLine = "\n"

sInline :: Serializer b i (Inline i)
sInline = over \case
  Word x -> x >$ sEscaped
  Number x -> x >$ sEscapedEnd
  Comma -> ","
  Colon -> ":"
  SemiColon -> ";"
  Verbatim txt -> "`" <> (txt >$ sText) <> "`"
  Reference txt -> "@" <> (txt >$ sText)
  Qouted q -> q >$ sQoutedSentences

sInlineSep :: Serializer b i (i, i)
sInlineSep = Serial (\cfg -> runSerializer (sCfgInlineSep cfg) cfg)

sBlockSep :: Serializer b i (b, b)
sBlockSep = Serial (\cfg -> runSerializer (sCfgBlockSep cfg) cfg)

sI :: Serializer b i i
sI = Serial (\cfg i -> runSerializer (sCfgInline cfg) cfg i)

sB :: Serializer b i b
sB = Serial (\cfg b -> runSerializer (sCfgBlock cfg) cfg b)

sBlocks :: Serializer b i (NE.NonEmpty b)
sBlocks = over \(r NE.:| rest) ->
  (sB $< r)
  <> foldMap
    (\p@(_, r2) -> (sBlockSep $< p) <> (sB $< r2))
    (zip (r:rest) rest)

sBlock :: Serializer b i (Block b i)
sBlock = over \case

  Para sb ->
    sIndent <> (sSentences $< sb) <> sEndLine

  Comment its ->
    foldMap
      (\i -> sIndent <> "--" <> (sText $< i) <> sEndLine)
      its

  Items itms -> itms >$ sWithCompressedItems \case
    Just trees ->
      foldMap (sItemTree $<) trees
    Nothing ->
      let (i NE.:| its) = itms
      in (sItem $< i) <> foldMap (\i' -> sEndLine <> (sItem $< i')) its

 where
  sItemTree :: Serializer b i (ItemTree i)
  sItemTree = sIndent <> over \(ItemTree (Item it _ tt blks)) -> indent $
     (sItemType $< it)
     <> (sSentences $< tt)
     <> sEndLine
     <> case NE.nonEmpty blks of
          Nothing -> mempty
          Just trees -> foldMap (sItemTree $<) trees

  sItem = sIndent <> over \(Item it _ tt blks) -> indent $
     (sItemType $< it)
     <> (sSentences $< tt)
     <> sEndLine
     <> case NE.nonEmpty blks of
          Nothing -> mempty
          Just blks' ->
            sEndLine <> (sBlocks $< blks')

  sItemType = over \case
    Minus -> "- "
    Plus  -> "+ "
    Times -> "* "

sQoutedSentences :: Serializer b i (QoutedSentences i)
sQoutedSentences = over \(QoutedSentences qoute sens) ->
  let qouteit f t = f <> (sens >$ sSentences) <> t
  in case qoute of
    DoubleQoute -> qouteit "\"" "\""
    Parenthesis -> qouteit "(" ")"
    Emph -> qouteit "/" "/"
    Strong -> qouteit "*" "*"

sSentences :: Serializer b i (Sentences i)
sSentences = over \case
  OpenSentence inlines ->
    sInlines $< inlines
  ClosedSentence s rest -> case rest of
    Just m -> (sSentence $< s) <> sSentenceSep <> (sSentences $< m)
    Nothing -> sSentence $< s


sInlines' :: Serializer b i [i]
sInlines' = NE.nonEmpty >$< over \case
  Just x -> sInlines $< x
  Nothing -> mempty

sInlines :: Serializer b i (NE.NonEmpty i)
sInlines = over \(r NE.:| rest) ->
  (sI $< r)
  <> foldMap
    (\p@(_, r2) -> (sInlineSep $< p) <> (sI $< r2))
    (zip (r:rest) rest)

sSentence :: Serializer b i (Sentence i)
sSentence = overF \(Sentence i ends) ->
  [ sInlines $< i
  , foldMap (sEnd $<) ends
  ]

sEnd :: Serializer b i End
sEnd = over \case
  Exclamation -> "!"
  Question -> "?"
  Period -> "."

sSection :: (Int -> Serializer b i s) -> Int -> Serializer b i (Section s b i)
sSection sS n = over \Section {..} ->
  stimes n "#" <> " " <> singleline (sSentences $< sectionTitle) <> sEndLine
  <> (case NE.nonEmpty sectionContent of
    Just blks ->
      sEndLine <> (sBlocks $< blks)
    Nothing -> mempty
  )
  <> foldMap (\s -> sEndLine <> (sS (n+1) $< s)) sectionSubs

type SimpleSerializer = Serializer Block' Inline'

sSimpleSection :: Int -> SimpleSerializer Section'
sSimpleSection n = getSection >$< sSection sSimpleSection n

sDoc :: SimpleSerializer Section'
sDoc = sSimpleSection 1

simpleSerializeConfig :: SerialConfig Block' Inline'
simpleSerializeConfig = SerialConfig
  { sCfgInline = getInline >$< sInline
  , sCfgBlock = getBlock >$< sBlock
  , sCfgSingleLine = False
  , sCfgIndent = 0
  , sCfgInlineSep = over \(_, i) -> case getInline i of
      Comma -> mempty
      SemiColon -> mempty
      Colon -> mempty
      Qouted p
        | countSentencesInQouted countSentencesInInline p > 1 -> sSentenceSep
      _ -> " "
  , sCfgBlockSep = over \case
    (Block' (Items _), Block' (Items _)) -> sEndLine <> sEndLine
    _ -> sEndLine
  , sCfgCompressItem = compressItem'
  }

runSimpleSerializer :: SimpleSerializer a -> a -> Text.Text
runSimpleSerializer = serialize simpleSerializeConfig
