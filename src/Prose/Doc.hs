module Prose.Doc where

-- base
import Prelude hiding (Word)

-- text 
import qualified Data.Text as Text

type Doc = SimpleSection

newtype SimpleSection = SimpleSection 
  { getSection :: Section SimpleSection SimpleBlock Inline }
  deriving (Eq)

newtype SimpleBlock = SimpleBlock
  { getBlock :: Block SimpleBlock Inline }
  deriving (Eq)

instance Show SimpleSection where
  showsPrec n (SimpleSection s) = 
    showParen (n > app_prec) (showString "smpl " . showsPrec (app_prec + 1) s)
   where app_prec = 10

instance Show SimpleBlock where
  showsPrec n (SimpleBlock s) = 
    showParen (n > app_prec) (showString "smpl " . showsPrec (app_prec + 1) s)
   where app_prec = 10

-- | A Section
data Section s b i = Section
  { sectionTitle :: [Sentence i]
  , sectionContent :: [b]
  , sectionSubs :: [s]
  } deriving (Show, Eq)

-- | A Block
newtype Block b i
  = Para [Sentence i]
  deriving (Eq, Show)

data Inline 
  = Word Text.Text
  | Comma
  | Colon
  | SemiColon
  | Hyphen
  deriving (Eq, Show)

data Sentence i = Sentence
  { sentenceContent :: [i]
  , sentenceEnd :: [End]
  }
  deriving (Eq, Show)

data End
  = Exclamation
  | Question
  | Period
  deriving (Eq, Show)
