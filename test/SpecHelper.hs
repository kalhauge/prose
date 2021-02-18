module SpecHelper 
  ( it
  , describe
  , Spec, SpecWith, runIO
  , module Test.Hspec.Hedgehog
  , module Test.Hspec.Expectations.Pretty
  , prop
  ) where

import Test.Hspec
import Test.Hspec.Hedgehog
import Test.Hspec.Expectations.Pretty

prop :: HasCallStack => String -> PropertyT IO () -> SpecWith ()
prop msg = it msg . hedgehog
