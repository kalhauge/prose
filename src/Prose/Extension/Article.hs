{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Prose.Extension.Article where

-- parser-combinators
-- import Control.Monad.Combinators

-- pandoc-type
import Text.Pandoc.Builder qualified as P

-- megaparsec
import Text.Megaparsec

-- mtl
import Control.Monad.State

-- base
import Data.Void
import Data.List.NonEmpty qualified as NE

-- text
import Data.Text qualified as Text

import Prose.Builder
import Prose.Doc
import Prose.Pandoc
import Prose.Text.Serializer
import Prose.Internal.Validation
import Prose.Internal.DocParser

data Author = Author
  { authorName :: Text.Text
  , authorAffiliation :: Maybe Text.Text
  }
  deriving (Eq, Show)

data Article = Article
  { articleTitle :: Sentences Inline'
  , articleAuthors :: [ Author ]
  , articleAbstract :: [ Sentences Inline' ]
  , articleSections :: [ Section' ]
  }
  deriving (Eq, Show)

data Error = Error
  deriving (Eq, Show)

type BlockParser =
  StateT [Block Block' Inline'] (Validation [Error])


fromDoc :: Section' -> Either (ParseErrorBundle [Block'] Void) Article
fromDoc (Section' Section {..}) = do
  let articleTitle = sectionTitle
  let articleSections = sectionSubs

  let
    x = runParser do
      articleAuthors <- maybe [] getAuthors <$> optional dCompressedItems
      articleAbstract <- many dPara
      pure $ Article {..}

  x "hello" sectionContent

 where
  getAuthors :: Foldable f => f (ItemTree Inline') -> [ Author ]
  getAuthors = foldMap \(ItemTree (Item _ _ t x)) ->
    concat [ flip foldMap x \case
      ItemTree (Item _ _ n []) -> [Author (serialize n) Nothing]
      _ -> error "unmathced"
    | t == sen (sb [word' "by", colon' ])
    ]


toDoc :: Article -> Section'
toDoc Article {..} = Section' $ Section
  { sectionTitle = articleTitle
  , sectionContent = concat
    [ case NE.nonEmpty articleAuthors of
        Just _ -> [ items'
            [ item' (sb [ word' "by", colon' ])
              [ items'
                [ item' (sb [ word' n]) []
                | Author n _ <- articleAuthors
                ]
              ]
            ]
          ]
        Nothing -> []
    , Block' . para <$> articleAbstract
    , []
    ]
  , sectionSubs = articleSections
  }

toPandoc :: Article -> P.Pandoc
toPandoc Article {..} =
  P.setTitle (toPandocSentences toI articleTitle)
  . P.setAuthors
    (authorToPandoc <$> articleAuthors)
  . P.setMeta "abstract"
    (foldMap (toB . Block' . Para) articleAbstract)
  . P.doc
  $ foldMap (toS 1) articleSections

 where
  authorToPandoc Author {..} =
    P.text authorName

  toI = toPandocInline toI . getInline
  toB = toPandocBlock toB toI . getBlock
  toS n = toPandocSection toS toB toI n . getSection

