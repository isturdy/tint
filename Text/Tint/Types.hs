module Text.Tint.Types where

import           Data.Colour
import           Data.Text

data TxtClass = NRM -- ^ Normal text; unhighlit
              | FUN -- ^ Function
              | CMT -- ^ Comment
              | PRG -- ^ Pragma (directive)
              | STR -- ^ String
              | OP  -- ^ Operator
              | KWD -- ^ Keyword
              | TYP -- ^ Datatype
              | OT1 -- ^ Miscellaneous 1
              | OT2 -- ^ Miscellaneous 2
                deriving (Show)

txtClasses :: [TxtClass]
txtClasses = [NRM, FUN, CMT, PRG, STR, OP, KWD, TYP, OT1, OT2]

data CodeBlock = CodeBlock {
    language :: Text
  , classes  :: [Text]
  , content  :: [SubBlock]
  }

data SubBlock = Line [Code]
              | Sub  CodeBlock

data Code = Code TxtClass Text

-- | The representation of colour schemes.
data Colours = Colours {
    background :: DColour
  , foreground :: (TxtClass -> DColour) -- ^ MUST be total.
  }

type DColour = Colour Double
