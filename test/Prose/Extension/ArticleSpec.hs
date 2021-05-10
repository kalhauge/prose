{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Prose.Extension.ArticleSpec where

-- hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- megaparsec
import Text.Megaparsec (initialPos)

-- prose
import Prose.Builder
import Prose.Doc
import Prose.Simple
import Prose.Annotated
import Prose.Recursion
import Prose.Extension.Article
import Prose.Text.Parser

import Prose.Internal.Validation

-- prose-test
import Prose.DocSpec
import SpecHelper

spec :: Spec
spec = do
  it "should read article" do
    Right i <- parseFile "test/data/canonical/article.prs" 

    s <- case fromDoc i of
      Success s -> return s
      Failure e -> error (show e)

    toDoc s `shouldBe` overSec toSimple i

    -- let Success art = fromDoc i
    --     Success art2 = fromDoc (overSec (fromSimple (initialPos "nofile")) (toDoc art)) 

    --art `shouldBe` art2




--   prop "should convert to Doc" do
--     i <- forAll genArticle
--     tripping i toDoc (\(an :: Section') ->
--         fromDoc 
--         (overSec (fromSimple (initialPos "nofile")) an)
--       )
-- 
-- genArticle :: MonadGen m =>  m Article
-- genArticle = do
--   _articleTitle <- toSentences genSimple
--   _articleAuthors <- Gen.list (Range.linear 0 3) genAuthor
--   _articleAbstract <- Gen.list (Range.linear 0 3) (toSentences genSimple)
--   _articleSections <- Gen.list (Range.linear 0 2) (onSec genSimpleR)
-- 
--   pure $ Article {..}
-- 
-- genAuthor :: MonadGen m => m Author
-- genAuthor = Author
--   <$> Gen.element [ "Harry Potter", "Ron Weasley", "Hermione Granger" ]
--   <*> Gen.constant Nothing -- Gen.maybe (Gen.constant "Hogwards")


--genLoremIpsum :: MonadGen m => m (Sentences Inline')
--genLoremIpsum = Gen.constant
--  (sen' $ sb [ Word "Lorem", Word "ipsum", Word "dolor", Word "sit", Word "amet" ] <. [])
--  where
--   loremIpsum =
--     [sen|Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
-- Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
-- Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
-- Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.|]
