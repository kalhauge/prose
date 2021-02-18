{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
-- Parse a doc from Markdown.
module Prose.Text.Markdown where

-- base
import Data.Void
import Data.Functor
import Data.Foldable
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

pInline :: Parser Inline
pInline = choice 
  [ Word <$> takeWhile1P (Just "a word") (isAlpha)
  ]

pBlock :: Parser SimpleBlock
pBlock = SimpleBlock <$> do
  notFollowedBy (single '#')
  para <- choice 
    [ Para <$> fold 
      [ concat <$> some ( 
        choice 
          [ (:[]) <$> pInline 
          , [] <$ try (eol <* notFollowedBy eol)
          ]
        )
      ]
    ]
  void (eol <* some eol) <|> eof
  return para

pSection :: Int -> Parser SimpleSection
pSection level = do 
  _ <- count level $ single '#'
  _ <- hspace
  title <- some pInline
  _ <- eol
  blocks <- many pBlock
  sections <- many $ pSection (level + 1)
  return $ SimpleSection (Section title blocks sections)

pDoc :: Parser SimpleSection
pDoc = pSection 1 <* eof

type Serialized = S.Doc ()

serialize :: Serialized -> Text.Text
serialize = S.renderStrict . S.layoutPretty S.defaultLayoutOptions 

sDoc :: SimpleSection -> Serialized
sDoc = sSection 1

sSection :: Int -> SimpleSection -> Serialized
sSection n (SimpleSection sec) = S.vsep
  [ stimes n "#" S.<+> sInlines (sectionTitle sec)
  , sBlocks (sectionContent sec)
  , S.vsep [ sSection (n + 1) s | s <- sectionSubs sec ]
  ]

sBlocks :: [SimpleBlock] -> Serialized
sBlocks = S.vsep . map sBlock

sBlock :: SimpleBlock -> Serialized
sBlock (SimpleBlock block) = case block of 
  Para sb -> sInlines sb <> S.hardline <> S.hardline

sInlines :: [Inline] -> Serialized
sInlines = S.hsep . map sInline 

sInline :: Inline -> Serialized
sInline = \case
  Word x -> S.pretty x

