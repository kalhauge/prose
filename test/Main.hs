{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Main where

-- base 
import Data.Functor
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Exit
import System.IO
import qualified Control.Exception as E

-- tasty
import Test.Tasty

-- tasty-hspec
import Test.Tasty.Hspec

-- tasty-rerun
import Test.Tasty.Ingredients.Rerun

-- glob
import System.FilePath.Glob (glob)

-- doctest
import Test.DocTest (doctest)

-- hlint
import Language.Haskell.HLint

import qualified Spec 

main :: IO ()
main = do
  m <- concat <$> sequence
    [ specifications
    , doctests
    , hlints
    ]

  defaultMainWithRerun (testGroup "All" m)

 where
  specifications :: IO [TestTree]
  specifications = (:[]) <$> testSpec "Specifications" 
    Spec.spec

  doctests :: IO [TestTree]
  doctests = testSpecs $ it "Doctest" do
    captureDoctest =<< glob "src/**/*.hs"

  hlints :: IO [TestTree]
  hlints = testSpecs $ it "HLint" do
    hlint ["src", "test", "--quiet"] >>= \case 
      [] -> return ()
      a:_ -> expectationFailure (showIdeaANSI a)

-- | capture the doctests
captureDoctest :: [String] -> IO ()
captureDoctest files = withFile ".doctest" ReadWriteMode \h ->
  E.catch
    ( redirectingHandle stderr h (doctest files) )
    ( \case
        ExitFailure _ -> do
          hFlush h
          hSeek h AbsoluteSeek 0
          expectationFailure =<< hGetContents h
        ExitSuccess -> do
          return ()
    )

-- | redirectingHandle; borrowed from 
-- https://hackage.haskell.org/package/main-tester-0.2.0.1/docs/Test-Main.html
redirectingHandle :: Handle -> Handle -> IO r -> IO r
redirectingHandle from to action = E.bracket 
  ( hDuplicate from >>= (hDuplicateTo to from $>) )
  ( \save -> hDuplicateTo save from >> hClose save )
  ( const action )

