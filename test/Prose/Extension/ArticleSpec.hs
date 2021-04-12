-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Prose.Extension.ArticleSpec where

-- hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- prose

import Prose.Builder
import Prose.Doc
import Prose.Extension.Article

-- prose-test
import Prose.DocSpec
import SpecHelper

spec :: Spec
spec = do
  return ()

--   prop "should convert to Doc" do
--     i <- forAll genArticle
--     tripping i toDoc fromDoc
--
--
-- genArticle :: MonadGen m =>  m Article
-- genArticle = do
--   articleTitle <- generate $ genSentences genSimpleInline
--   articleAuthors <- Gen.list (Range.linear 0 3) genAuthor
--   articleAbstract <- Gen.list (Range.linear 0 3) genLoremIpsum
--   articleSections <- Gen.list (Range.linear 0 2) $ generate genSimpleSection
--
--   pure $ Article {..}
--
-- genAuthor :: MonadGen m => m Author
-- genAuthor = Author
--   <$> Gen.element [ "Harry Potter", "Ron Weasley", "Hermione Granger" ]
--   <*> Gen.constant Nothing -- Gen.maybe (Gen.constant "Hogwards")
--
--
-- genLoremIpsum :: MonadGen m => m (Sentences Inline')
-- genLoremIpsum = Gen.constant
--   (sen' $ sb [ Word "Lorem", Word "ipsum", Word "dolor", Word "sit", Word "amet" ] <. [])
-- --  where
-- --   loremIpsum =
-- --     [sen|Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
-- -- Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
-- -- Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
-- -- Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.|]
