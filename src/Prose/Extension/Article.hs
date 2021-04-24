{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Prose.Extension.Article where

-- base
import Control.Applicative hiding (many, some)
import Data.Sequence qualified as Seq

-- pandoc-type
import Text.Pandoc.Builder qualified as P

-- parser-combinators
import Control.Monad.Combinators

-- lens
import Control.Lens hiding (Simple, para)

-- mtl
import Control.Monad.State

-- base
import Data.List.NonEmpty qualified as NE

-- text
import Data.Text qualified as Text

import Prose.Annotated
import Prose.Builder
import Prose.Doc
import Prose.Internal.DocParser
import Prose.Internal.Validation
import Prose.Pandoc
import Prose.Recursion
import Prose.Simple

data Author = Author
  { _authorName :: Text.Text
  , _authorAffiliation :: Maybe Text.Text
  }
  deriving (Eq, Show)

data Article = Article
  { _articleTitle :: Sentences Simple
  , _articleAuthors :: [Author]
  , _articleAbstract :: [Sentences Simple]
  , _articleSections :: [Section']
  }
  deriving (Eq, Show)

makeLenses ''Article
makeLenses ''Author

fromDoc :: AnnSection MPos -> Validation Fault Article
fromDoc doc = do
  let sec = doc ^. unAnnSec
      _articleTitle = mapDoc toSimple (sec ^. sectionTitle)
      _articleSections =
        sec ^.. sectionSubs . folded . unAnnSec . to (mapDoc toSimple) . colSec

      content = sec ^. sectionContent

  flip parseAll content do
    let _articleAuthors = [] -- getAuthors <$> dItemTree
    _articleAbstract <- many dPara
    return Article{..}

--  where
--   getAuthors :: Foldable f => f (ItemTree Simple) -> [ Author ]
--   getAuthors = foldMap \(ItemTree _ _ t x) ->
--     concat [ flip foldMap x \case
--       ItemTree _ _ n [] -> [Author (serialize n) Nothing]
--       _ -> error "unmathced"
--     | t == sen (sb [word' "by", colon' ])
--     ]

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

toDoc :: Article -> Section'
toDoc Article{..} =
  Section' $
    Section
      { _sectionTitle = _articleTitle
      , _sectionContent =
          concat
            [ case NE.nonEmpty _articleAuthors of
                Just _ ->
                  [ items'
                      [ item'
                          (sb [word' "by", colon'])
                          [ items'
                              [ item' (sb [word' n]) []
                              | Author n _ <- _articleAuthors
                              ]
                          ]
                      ]
                  ]
                Nothing -> []
            , Block' . para <$> _articleAbstract
            , []
            ]
      , _sectionSubs = _articleSections
      }

toPandoc :: Article -> P.Pandoc
toPandoc Article{..} =
  P.setTitle (pandocSentences $ mapDoc simpleToPandoc _articleTitle)
    . P.setAuthors
      (authorToPandoc <$> _articleAuthors)
    . P.setMeta
      "abstract"
      (foldMap (overBlk simpleToPandoc . Block' . Para) _articleAbstract)
    . P.doc
    $ foldMap (\s -> overSec simpleToPandoc s 1) _articleSections
 where
  authorToPandoc Author{..} =
    P.text _authorName
