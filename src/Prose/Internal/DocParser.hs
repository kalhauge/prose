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

-- pandoc
import qualified Text.Pandoc as PD

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
  = EndOfList Text.Text
  | ItemsLeftInList
  | CouldNotMatch Text.Text
  | APandocError PD.PandocError
  | FaultIn Text.Text MPos (Seq.Seq Fault)
  deriving (Show)

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

pop :: Text.Text -> DocParser b b
pop txt =
  validate =<< state \case
    [] -> (Failure (pure (EndOfList txt)), [])
    a : rest -> (Success a, rest)

try :: DocParser b a -> DocParser b a
try ps = DocParser (ValidationT $ state (\s ->  
  case runState (runValidationT (runDocParser ps)) s of
    (Success a, s') -> (return a, s')
    (Failure msg, _) -> (Failure msg, s)
  ))

ptail :: DocParser b [b]
ptail = state (,[])

newtype FirstV a = FirstV {runFirstV :: Validation Fault a}

instance Semigroup (FirstV a) where
  (FirstV a) <> b = case a of
    Success _ -> FirstV a
    Failure _ -> b

instance Monoid (FirstV a) where
  mempty = FirstV (Failure mempty)

match :: Text.Text -> (b -> Validation Fault a) -> DocParser b a
match txt getter = try (validate . getter =<< pop txt)

matchSec :: (Section APos -> Validation Fault a) -> DocParser (Sec APos) a
matchSec getter = try $ do
  AnnSection _ sec <- pop "section"
  validate $ getter sec

submatch ::
  (b -> Seq.Seq Fault -> Seq.Seq Fault)
  -> (b -> Validation Fault [a]) 
  -> DocParser a c 
  -> DocParser b c
submatch f getter pac = try $ do
  b <- pop "submatch"
  as <- validate (getter b)
  case parseAll pac as of
    Success a -> return a
    Failure err -> validate $ Failure (f b err)

submatchSec ::
  (Sec APos -> Validation Fault [a]) 
  -> DocParser a c 
  -> DocParser (Sec APos) c
submatchSec = do
  submatch (\(AnnSection _ _) -> id)

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
matchL l = match "takeit" (takeIt l)

type ParseLens b a = Getting (FirstV a) b a

-- inBlk :: ParseLens (Blk APos) (Block APos)
-- inBlk fn (AnnBlock a b) = fn b <&> \case
--   Success x -> Success x
--   Failure err -> Failure (pure $ FaultIn "block" a err)

dCodeBlock :: DocParser (Blk APos) (Maybe Text.Text, [Text.Text])
dCodeBlock = match "code-block" (takeIt $ unAnnBlk . _CodeBlock)

dPara :: DocParser (Blk APos) (Sentences Simple)
dPara = match "para" (takeIt $ unAnnBlk .  _Para . to (mapDoc toSimple))

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
