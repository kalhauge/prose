{-# LANGUAGE BlockArguments #-}
module SpecHelper (
  it,
  describe,
  Spec,
  SpecWith,
  runIO,
  focus,
  module Test.Hspec.Hedgehog,
  module Test.Hspec.Expectations.Pretty,
  prop,
  
  onFile,
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Test.Hspec
import Test.Hspec.Expectations.Pretty
import Test.Hspec.Hedgehog

prop :: HasCallStack => String -> PropertyT IO () -> SpecWith ()
prop msg = it msg . hedgehog

onFile :: FilePath -> (Text.Text -> SpecWith ()) -> SpecWith ()
onFile file k = describe file do
  k =<< runIO (Text.readFile file)
  return ()
