{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prose.Extension.Article where

-- parser-combinators
-- import Control.Monad.Combinators

-- pandoc-type
import Text.Pandoc.Builder qualified as P

-- megaparsec
-- import Text.Megaparsec

-- mtl
import Control.Monad.State

-- base
import Data.List.NonEmpty qualified as NE

-- text
import Data.Text qualified as Text

import Prose.Builder
import Prose.Doc
import Prose.Pandoc
import Prose.Recursion
import Prose.Simple

-- import Prose.Text.Serializer
import Prose.Internal.Validation

-- import Prose.Internal.DocParser

data Author = Author
  { authorName :: Text.Text
  , authorAffiliation :: Maybe Text.Text
  }
  deriving (Eq, Show)

data Article = Article
  { articleTitle :: Sentences Simple
  , articleAuthors :: [Author]
  , articleAbstract :: [Sentences Simple]
  , articleSections :: [Section']
  }
  deriving (Eq, Show)

data Error = Error
  deriving (Eq, Show)

type BlockParser =
  StateT [Block'] (Validation [Error])

-- fromDoc :: Section' -> Either (ParseErrorBundle [ABlock] Void) Article
-- fromDoc (Section' Section {..}) = do
--   let articleTitle = mapDoc toSimple sectionTitle
--   let articleSections = sectionSubs
--
--   let
--     x = runParser do
--       articleAuthors <- maybe [] getAuthors <$> optional dCompressedItems
--       articleAbstract <- many dPara
--       pure $ Article {..}
--
--   x "hello" sectionContent
--
--  where
--   getAuthors :: Foldable f => f (ItemTree Simple) -> [ Author ]
--   getAuthors = foldMap \(ItemTree _ _ t x) ->
--     concat [ flip foldMap x \case
--       ItemTree _ _ n [] -> [Author (serialize n) Nothing]
--       _ -> error "unmathced"
--     | t == sen (sb [word' "by", colon' ])
--     ]

toDoc :: Article -> Section'
toDoc Article{..} =
  Section' $
    Section
      { _sectionTitle = articleTitle
      , _sectionContent =
          concat
            [ case NE.nonEmpty articleAuthors of
                Just _ ->
                  [ items'
                      [ item'
                          (sb [word' "by", colon'])
                          [ items'
                              [ item' (sb [word' n]) []
                              | Author n _ <- articleAuthors
                              ]
                          ]
                      ]
                  ]
                Nothing -> []
            , Block' . para <$> articleAbstract
            , []
            ]
      , _sectionSubs = articleSections
      }

toPandoc :: Article -> P.Pandoc
toPandoc Article{..} =
  P.setTitle (pandocSentences $ mapDoc simpleToPandoc articleTitle)
    . P.setAuthors
      (authorToPandoc <$> articleAuthors)
    . P.setMeta
      "abstract"
      (foldMap (overBlk simpleToPandoc . Block' . Para) articleAbstract)
    . P.doc
    $ foldMap (\s -> overSec simpleToPandoc s 1) articleSections
 where
  authorToPandoc Author{..} =
    P.text authorName
