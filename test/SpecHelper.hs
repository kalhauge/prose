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
) where

import Test.Hspec
import Test.Hspec.Expectations.Pretty
import Test.Hspec.Hedgehog

prop :: HasCallStack => String -> PropertyT IO () -> SpecWith ()
prop msg = it msg . hedgehog
