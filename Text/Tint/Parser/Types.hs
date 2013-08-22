module Text.Tint.Parser.Types where

import           Data.Text

import           Text.Tint.Types

data ConChange = Stay
               | Pop
               | Push Con
               | CList [ConChange]

data Con = Con Text
         | ParamCon Text Text

data Syntax = Syntax {
    lists    :: [(Text, Text)]
  , contexts :: [Context]
  , matches  :: [Match]
  }

data Context = Context {
    contextId   :: Text
  , defClass    :: TxtClass
  , lineEndCon  :: ConChange
  , fallThrough :: Maybe ConChange
  }

data Match = Match {
    matchType  :: MatchType
  , matchClass :: [TxtClass]
  , matchCon   :: ConChange
  }

data MatchType = Str Text
               | Regex Text
               | ParamRegex (Text -> Text)
               | Keyword Text
               | Int
               | Float
