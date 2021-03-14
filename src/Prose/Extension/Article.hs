{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Prose.Extension.Article where

-- mtl
import Control.Monad.State

-- base
import Data.List.NonEmpty qualified as NE

import Prose.Builder
import Prose.Doc
import Prose.Internal.Validation

data Author = Author
  { authorName :: String
  , authorAffiliation :: Maybe String
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


popBlock :: BlockParser (Block Block' Inline')
popBlock = get >>= \case
  [] -> lift (Failure [Error])
  a:s -> put s >> return a

getCompressedItems :: BlockParser (NE.NonEmpty (ItemTree Inline'))
getCompressedItems =  popBlock >>= \case
  Items is -> do
    let Just trees = mapM compressItem' is
    return trees
  _ -> lift (Failure [Error])

getPara :: BlockParser (Sentences Inline')
getPara =  popBlock >>= \case
  Para is -> return is
  _ -> lift (Failure [Error])


fromDoc :: Section' -> Validation [Error] Article
fromDoc (Section' Section {..}) = do
  let articleTitle = sectionTitle
  let articleSections = sectionSubs

  flip evalStateT sectionContent do
    let articleAbstract = []
    let articleAuthors = []
    pure $ Article {..}

toDoc :: Article -> Section'
toDoc Article {..} = Section' $ Section
  { sectionTitle = articleTitle
  , sectionContent =
    [ items'
      [ item' (sb [ word' "by", colon' ]) []
      ]
    ]
  , sectionSubs = articleSections
  }
