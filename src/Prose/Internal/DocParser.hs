{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE ScopedTypeVariables #-}
module Prose.Internal.DocParser where

-- base
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import qualified Data.Sequence as Seq

-- lens
import Control.Lens hiding (Simple)

-- megaparsec
import Text.Megaparsec (SourcePos)

import qualified Data.Text as Text

-- mtl
import Control.Monad.State

import Prose.Annotated
import Prose.Doc
import Prose.Internal.Validation
import Prose.Recursion
import Prose.Simple (Simple)

-- type DocParser i = Parsec Void [i]

type APos = Ann MPos
type ABlock = AnnBlock MPos

type MPos = SourcePos

data Fault
  = EndOfList
  | ItemsLeftInList
  | CouldNotMatch Text.Text
  | FaultIn Text.Text MPos (Seq.Seq Fault)
  deriving (Eq, Show)

type DocParserInt b =
  ValidationT (State [b]) Fault

newtype DocParser b a = DocParser {runDocParser :: DocParserInt b a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState [b]
    , MonadValidate Fault
    , Alternative
    , MonadPlus
    )
    via (DocParserInt b)

parseAll :: DocParser b a -> [b] -> Validation Fault a
parseAll (DocParser s) bs = case runState (runValidationT s) bs of
  (a, []) -> a
  (Failure err, _ : _) -> Failure err
  (_, _ : _) -> Failure (pure ItemsLeftInList)

pop :: DocParser b b
pop =
  validate =<< state \case
    [] -> (Failure (pure EndOfList), [])
    a : rest -> (Success a, rest)

ptail :: DocParser b [b]
ptail = state (,[])

newtype FirstV a = FirstV {runFirstV :: Validation Fault a}

instance Semigroup (FirstV a) where
  (FirstV a) <> b = case a of
    Success _ -> FirstV a
    Failure _ -> b

instance Monoid (FirstV a) where
  mempty = FirstV (Failure mempty)

match :: (b -> Validation Fault a) -> DocParser b a
match getter = validate . getter =<< pop

submatch ::
  (b -> Seq.Seq Fault -> Seq.Seq Fault)
  -> (b -> Validation Fault [a]) 
  -> DocParser a c 
  -> DocParser b c
submatch f getter pac = do
  b <- pop
  as <- validate (getter b)
  case parseAll pac as of
    Success a -> return a
    Failure err -> validate $ Failure (f b err)

submatchBlk ::
  (Blk APos -> Validation Fault [a]) 
  -> DocParser a c 
  -> DocParser (Blk APos) c
submatchBlk =
  submatch (\(AnnBlock an blk) -> pure . FaultIn (case blk of
   (Para _) -> "Para"
   (Comment _) -> "Comment"
   (Items _) -> "Items"
   (CodeBlock _ _) -> "CodeBlock" 
   (OrderedItems _ _) -> "OrderedItems") an)

takeIt :: Getting (Data.Monoid.First a) b a -> (b -> Validation Fault a)
takeIt l b = case b ^? l of
  Just a -> Success a
  Nothing -> Failure mempty

matchL :: Getting (Data.Monoid.First a) b a -> DocParser b a
matchL l = match (takeIt l)

type ParseLens b a = Getting (FirstV a) b a

-- inBlk :: ParseLens (Blk APos) (Block APos)
-- inBlk fn (AnnBlock a b) = fn b <&> \case
--   Success x -> Success x
--   Failure err -> Failure (pure $ FaultIn "block" a err)

dPara :: DocParser (Blk APos) (Sentences Simple)
dPara = match (takeIt $ unAnnBlk .  _Para . to (mapDoc toSimple))

dItems ::
  DocParser (Item APos) a ->
  DocParser (Blk APos) a
dItems =
  submatchBlk (takeIt $ unAnnBlk . _Items . to NE.toList)

dCompressedItems ::
  DocParser (ItemTree APos) a ->
  DocParser (Blk APos) a
dCompressedItems =
  submatchBlk (takeIt $ to compressItems . _Just . to NE.toList)
