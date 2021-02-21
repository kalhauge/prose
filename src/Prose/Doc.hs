module Prose.Doc where

-- base
import Prelude hiding (Word)
import qualified Data.List.NonEmpty as NE

-- text 
import qualified Data.Text as Text

type Doc = SimpleSection

newtype SimpleSection = SimpleSection 
  { getSection :: Section SimpleSection SimpleBlock SimpleInline }
  deriving (Eq)

newtype SimpleBlock = SimpleBlock
  { getBlock :: Block SimpleBlock SimpleInline }
  deriving (Eq)

newtype SimpleInline = SimpleInline
  { getInline :: Inline SimpleInline }
  deriving (Eq)

instance Show SimpleSection where
  showsPrec n (SimpleSection s) = 
    showParen (n > app_prec) (showString "smpl " . showsPrec (app_prec + 1) s)
   where app_prec = 10

instance Show SimpleBlock where
  showsPrec n (SimpleBlock s) = 
    showParen (n > app_prec) (showString "smpl " . showsPrec (app_prec + 1) s)
   where app_prec = 10

instance Show SimpleInline where
  showsPrec n (SimpleInline s) = 
    showParen (n > app_prec) (showString "smpl " . showsPrec (app_prec + 1) s)
   where app_prec = 10

-- | A Section
data Section s b i = Section
  { sectionTitle :: Sentences i
  , sectionContent :: [b]
  , sectionSubs :: [s]
  } deriving (Show, Eq)

-- | A Block
newtype Block b i
  = Para (Sentences i)
  deriving (Eq, Show)

data Sentences i = Sentences 
  { closed :: [Sentence i]
  , final :: [i]
  } 
  deriving (Eq, Show)

data Sentence i = Sentence
  { sentenceContent :: NE.NonEmpty i
  , sentenceEnd :: NE.NonEmpty End
  }
  deriving (Eq, Show)

data End
  = Exclamation
  | Question
  | Period
  deriving (Eq, Show, Enum, Bounded)

data QoutedSentences i = QoutedSentences 
  { qoutedType :: Qoute
  , qoutedSentences :: Sentences i
  } 
  deriving (Eq, Show)

data Qoute 
  = SingleQoute
  | DoubleQoute
  | Italic
  | Strong
  deriving (Eq, Show, Enum, Bounded)

data Inline i
  = Word Text.Text
  | Comma
  | Colon
  | SemiColon
  | Hyphen
  | Qouted (QoutedSentences i)
  deriving (Eq, Show)
