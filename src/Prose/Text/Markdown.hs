{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
-- Parse a doc from Markdown.
module Prose.Text.Markdown where

-- base
import Data.Void
import Data.Functor
import Data.Semigroup
import Data.Char hiding (Space)
-- import Data.Functor

-- text
import qualified Data.Text as Text

-- megaparsec
import Text.Megaparsec 
import Text.Megaparsec.Char

-- prettyprinter
import qualified Data.Text.Prettyprint.Doc as S
import qualified Data.Text.Prettyprint.Doc.Render.Text as S

import Prose.Doc

type Parser = Parsec Void Text.Text


pBlock :: Parser SimpleBlock
pBlock = SimpleBlock <$> do
  notFollowedBy (single '#')
  para <- choice 
    [ do 
        sens <- some (pMultiLineSentence pInline)
        return $ Para sens
    ]
  void (eol <* some eol) <|> eof
  return para

pSection :: Int -> Parser SimpleSection
pSection level = do 
  _ <- count level $ single '#'
  _ <- hspace
  title <- some (pSingleLineSentence pInline)
  _ <- eol
  blocks <- many pBlock
  sections <- many $ pSection (level + 1)
  return $ SimpleSection (Section title blocks sections)

pDoc :: Parser SimpleSection
pDoc = pSection 1 <* eof

type Serialized = S.Doc ()
type Serializer i = i -> Serialized

serialize :: Serialized -> Text.Text
serialize = S.renderStrict . S.layoutPretty S.defaultLayoutOptions 

sDoc :: SimpleSection -> Serialized
sDoc = sSection 1

sSection :: Int -> Serializer SimpleSection 
sSection n (SimpleSection sec) = S.vsep
  [ stimes n "#" S.<+> sSentences sInline (sectionTitle sec)
  , sBlocks (sectionContent sec)
  , S.vsep [ sSection (n + 1) s | s <- sectionSubs sec ]
  ]

sBlocks :: Serializer [SimpleBlock] 
sBlocks = S.vsep . map sBlock

sBlock :: Serializer SimpleBlock 
sBlock (SimpleBlock block) = case block of 
  Para sb -> sSentences sInline sb <> S.hardline <> S.hardline

sSentences :: Serializer i -> Serializer [Sentence i]
sSentences sInline' = 
  foldMap (S.hsep . map sInline' . sentenceContent) 


-- Inline

sInline :: Serializer Inline
sInline = \case
  Word x -> S.pretty x
  Comma -> ","
  Colon -> ":"
  SemiColon -> ";"
  Hyphen -> "-"

sInlineWithSpace :: Inline -> Serializer Inline
sInlineWithSpace i i2 = case i2 of
  Word _ -> (case i of Hyphen -> mempty; _ -> S.space) <> sInline i2
  _ -> sInline i2

pInline :: Parser Inline
pInline = choice 
  [ char ',' $> Comma
  , char ':' $> Colon
  , char ';' $> SemiColon
  , char '-' $> Hyphen 
  , Word <$> takeWhile1P (Just "a word") isAlphaNum
  ]

-- Sentence 

pEnd :: Parser End
pEnd = choice 
  [ char '!' $> Exclamation
  , char '?' $> Question
  , char '.' $> Period 
  ]

sEnd :: Serializer End
sEnd = \case 
  Exclamation -> "!"
  Question -> "?"
  Period -> "."

pSentence :: Parser () -> Parser i -> Parser (Sentence i)
pSentence pSpace pInline' = do 
  i <- some (pInline' <* pSpace)
  e <- many pEnd
  return (Sentence i e)

pSingleLineSentence :: Parser i -> Parser (Sentence i)
pSingleLineSentence = pSentence hspace

pMultiLineSentence :: Parser i -> Parser (Sentence i)
pMultiLineSentence = pSentence (hspace <* void (optional eol))

sSentence :: (i -> Serializer i) -> Serializer i -> Serializer (Sentence i)
sSentence sInlineWithSpace' sInline' (Sentence contents ends) = 
  content <> end
 where 
  content :: Serialized
  content = case contents of 
    c:rest -> 
      sInline' c <> 
        foldMap (uncurry sInlineWithSpace') (zip contents rest)
    [] -> mempty
  end = foldMap sEnd ends 

