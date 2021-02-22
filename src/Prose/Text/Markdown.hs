{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
-- Parse a doc from Markdown.
module Prose.Text.Markdown where

-- mtl
import Control.Monad.Reader

-- base
import Data.Void
import Data.String
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Char hiding (Space)

-- text
import qualified Data.Text as Text

-- megaparsec
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

-- parser-combinators
import Control.Monad.Combinators.NonEmpty

-- prettyprinter
import qualified Data.Text.Prettyprint.Doc as S
import qualified Data.Text.Prettyprint.Doc.Render.Text as S

import Prose.Doc

data ParserConfig s b i = ParserConfig
  { pInline' :: Parser s b i i 
  , pSpace :: Parser s b i ()
  , pActiveQoutes :: [Qoute]
  }

type Parser s b i = ParsecT Void Text.Text (Reader (ParserConfig s b i))

type SimpleParser = Parser SimpleSection SimpleBlock SimpleInline

simplePaserConfig :: ParserConfig SimpleSection SimpleBlock SimpleInline
simplePaserConfig = ParserConfig 
  { pInline' = SimpleInline <$> pInline
  , pSpace = hspace
  , pActiveQoutes = []
  }

pI :: Parser s b i i
pI = join (asks pInline')

type Serialized = S.Doc ()
type Serializer s b i a = a -> SerializeConfig s b i -> Serialized

data SerializeConfig s b i = SerializeConfig
  { sInline' :: Serializer s b i i
  , sInlineWithSpace' :: i -> Serializer s b i i
  }

instance IsString (SerializeConfig s b i -> Serialized) where
  fromString str = const (fromString str)


type SimpleSerializer a = Serializer SimpleSection SimpleBlock SimpleInline a

simpleSerializeConfig :: SerializeConfig SimpleSection SimpleBlock SimpleInline
simpleSerializeConfig = SerializeConfig 
  { sInline' = sInline . getInline
  , sInlineWithSpace' = \i i2 -> sInlineWithSpace (getInline i) (getInline i2) 
  }

serialize :: Serialized -> Text.Text
serialize = S.renderStrict . S.layoutPretty S.defaultLayoutOptions 

sI :: Serializer s b i i
sI i cfg = sInline' cfg i cfg

sIWS :: i -> Serializer s b i i
sIWS i1 i2 cfg = sInlineWithSpace' cfg i1 i2 cfg

-- Inline

sInline :: Serializer s b i (Inline i)
sInline i cfg = case i of
  Word x -> S.pretty x
  Comma -> ","
  Colon -> ":"
  SemiColon -> ";"
  Hyphen -> "-"
  Qouted s -> sQoutedSentences s cfg

sInlineWithSpace :: Inline i -> Serializer s b i (Inline i)
sInlineWithSpace i i2 = const spaces <> sInline i2
 where 
  spaces = case i2 of
    Word _ -> case i of 
      Hyphen -> mempty 
      _ -> S.space
    Qouted _ -> S.space
    _ -> mempty

pInline :: Show i => Parser s b i (Inline i)
pInline = label "inline" $ choice 
  [ char ',' $> Comma
  , char ':' $> Colon
  , char ';' $> SemiColon
  , char '-' $> Hyphen 
  , Qouted <$> pQoutedSentences 
  , Word <$> takeWhile1P (Just "a word") isAlphaNum
  ]

-- Sentence 

pEnd :: Parser s b i End
pEnd = label "end of sentence" $ choice 
  [ char '!' $> Exclamation
  , char '?' $> Question
  , char '.' $> Period 
  ]

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
  foldMap sSentence sens 
  <> sInlines rest 

sQoutedSentences :: Serializer s b i (QoutedSentences i)
sQoutedSentences (QoutedSentences qoute sentences) = case qoute of
  SingleQoute -> "'" <> sSentences sentences <> "'"
  DoubleQoute -> "\"" <> sSentences sentences <> "\""
  Parenthesis -> "(" <> sSentences sentences <> ")"
  Emph -> "/" <> sSentences sentences <> "/"
  Strong -> "*" <> sSentences sentences <> "*"


-- pSingleLineSentence :: Parser s b i (Sentence i)
-- pSingleLineSentence = local (\a -> a { pSpace = hspace }) pSentence
-- 
-- pMultiLineSentence :: Int -> Parser s b i (Sentence i)
-- pMultiLineSentence ident = 
--   local (\a -> a { pSpace = (hspace <* void (optional $ eol <* count ident (char ' ')))})
--   pSentence 

pSentences :: Show i => Parser s b i (Sentences i)
pSentences = do
  ParserConfig { pInline', pSpace } <- ask 
  let 
    go sens = choice 
      [ do
        i <- some (pInline' <* pSpace)
        ends <- many pEnd
        case NE.nonEmpty ends of
          Nothing -> return $ Sentences (reverse sens) (NE.toList i)
          Just ends' -> go (Sentence i ends':sens)
      , if not (null sens)
          then return $ Sentences (reverse sens) []
          else fail "not a sentence"
      ]
  go []

pQoutedSentences :: Show i => Parser s b i (QoutedSentences i)
pQoutedSentences = do 
  qoute <- startQoute
  sens <- local (\a -> a { pActiveQoutes = qoute: pActiveQoutes a}) pSentences 
  case qoute of 
    SingleQoute -> void $ label "end of qoute (')" (char '\'')
    DoubleQoute -> void $ label "end of qoute (\")" (char '\"')
    Emph -> void . label "end of emph (/)" $ char '/'
    Strong -> void . label "end of strong (*)" $ char '*'
    Parenthesis -> void . label "end of paranthesis ())" $ char ')'

  return $ QoutedSentences qoute sens

 where 
  startQoute = try $ do 
    qoutes <- asks pActiveQoutes
    x <-  choice 
      [ char '\'' $> SingleQoute
      , char '"' $> DoubleQoute
      , string "*" $> Strong
      , char '/' $> Emph
      , char '(' $> Parenthesis
      ] <* notFollowedBy (endQoute <|> void pEnd)

    if x `elem` qoutes
    then fail "Already included"
    else return x

  -- qouteSymbol = void $ oneOf ['\'', '"', '*']
  endQoute = void do 
    many (oneOf ['\'', '"', '*', '/']) <* (space1 <|> eof)

-- Blocks

-- sSentences :: Serializer i -> Serializer [Sentence i]
-- sSentences sInline' = 
--   foldMap (S.hsep . map sInline' . sentenceContent) 
-- 
-- sBlock :: Serializer SimpleBlock 
-- sBlock (SimpleBlock block) = case block of 
--   Para sb -> sSentences sInline sb <> S.hardline <> S.hardline
-- 
-- pBlock :: Parser i -> Parser (Block b i)
-- pBlock pInline' = do
--   notFollowedBy (single '#')
--   para <- choice 
--     [ do 
--         sens <- some (pMultiLineSentence 0 pInline' <* optional eol)
--         return $ Para sens
--     ]
--   void (try $ eol <* some eol) <|> eof
--   return para
-- 
-- -- Sections
-- 
-- pSection :: Int -> Parser SimpleSection
-- pSection level = do 
--   _ <- count level $ single '#'
--   _ <- hspace
--   title <- some (pSingleLineSentence pInline)
--   _ <- eol
--   blocks <- many (SimpleBlock <$> pBlock pInline)
--   sections <- many $ pSection (level + 1)
--   return $ SimpleSection (Section title blocks sections)
-- 
-- pDoc :: Parser SimpleSection
-- pDoc = pSection 1 <* eof
-- 
-- sDoc :: SimpleSection -> Serialized
-- sDoc = sSection 1
-- 
-- sSection :: Int -> Serializer SimpleSection 
-- sSection n (SimpleSection sec) = S.vsep
--   [ stimes n "#" S.<+> sSentences sInline (sectionTitle sec)
--   , sBlocks (sectionContent sec)
--   , S.vsep [ sSection (n + 1) s | s <- sectionSubs sec ]
--   ]
-- 
-- sBlocks :: Serializer [SimpleBlock] 
-- sBlocks = S.vsep . map sBlock


