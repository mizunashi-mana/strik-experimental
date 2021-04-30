module Language.Quell.Type.TextId (
  T,
  TextId (..),
  PrimTextId (..),
  primTextId,
  textId,
  stringLit,
  showByText,
) where

import           Language.Quell.Prelude


type T = TextId

newtype TextId = UnsafeTextId
    {
        unsafeUnTextId :: Text -- FIXME: Use memorized hash integer
    }
    deriving (Eq, Show)

data PrimTextId
    = PrimTextUnit
    | PrimTextArrow
    | PrimTextWildcard
    deriving (Eq, Show, Ord, Enum, Bounded)

primTextId :: PrimTextId -> TextId
primTextId = \case
    PrimTextUnit     -> stringLit "()"
    PrimTextArrow    -> stringLit "->"
    PrimTextWildcard -> stringLit "_"

textId :: Text -> TextId
textId txt = UnsafeTextId do txt

stringLit :: StringLit -> TextId
stringLit str = textId do text str

-- FIXME: Reference memorized hash table
showByText :: TextId -> Text
showByText = \case
  UnsafeTextId t -> t

-- FIXME: Consider comparing memorized hash integer mechanizm
instance Ord TextId where
  compare = coerce do compare @Text

instance Pretty TextId where
  pretty t = pretty do showByText t
