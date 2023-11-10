module Language.Strik.Model.ErrorCode (
  T,
  ErrorCode (..),
  ErrorLevel (..),
  toLevel,
) where

import           Language.Strik.Prelude


type T = ErrorCode

-- | TODO: split error code by components
data ErrorCode
  = Unknown

  -- lexer error
  | LexBreakEncoding
  | UnexpectedCodeUnits
  deriving (Eq, Ord, Bounded, Show)

instance Enum ErrorCode where
    toEnum = \case
        1 -> Unknown
        10000 -> LexBreakEncoding
        10001 -> UnexpectedCodeUnits
        i -> error do "Unknown value of ErrorCode: " <> show i

    fromEnum = \case
        Unknown -> 1
        LexBreakEncoding -> 10000
        UnexpectedCodeUnits -> 10001

data ErrorLevel
  = Bug
  | CriticalError
  | RecoverableError
  | Warning
  | Suggestion
  | Note
  | Message
  deriving (Eq, Ord, Enum, Bounded, Show)

toLevel :: ErrorCode -> ErrorLevel
toLevel = \case
  Unknown                 -> Bug
  LexBreakEncoding        -> RecoverableError
  UnexpectedCodeUnits     -> RecoverableError
