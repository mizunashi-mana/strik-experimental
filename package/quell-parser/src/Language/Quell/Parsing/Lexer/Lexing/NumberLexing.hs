module Language.Quell.Parsing.Lexer.Lexing.NumberLexing where

import           Language.Quell.Prelude

import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit

data LexedSign
    = LexedSignPositive
    | LexedSignNegative
    deriving (Eq, Ord, Bounded, Enum, Show)

lexSignChar :: CodeUnit.T -> Maybe LexedSign
lexSignChar = \case
    CodeUnit.LcGPlusSign -> Just LexedSignPositive
    CodeUnit.LcGHyphenMinus -> Just LexedSignNegative
    _                     -> Nothing

lexNumDotSymChar :: CodeUnit.T -> Bool
lexNumDotSymChar = \case
    CodeUnit.LcGFullStop -> True
    _ -> False

lexNumSepSymChar :: CodeUnit.T -> Bool
lexNumSepSymChar = \case
    CodeUnit.LcGLowLine -> True
    _ -> False

-- | Decode a digit
--
-- prop> case lexDigitChar x of { Nothing -> True; Just i -> 0 <= i && i < 10 }
--
-- * https://www.compart.com/en/unicode/category/Nd
--
-- TODO: Optimze branching.
{-# INLINE lexDigitChar #-}
lexDigitChar :: Char -> Maybe Int
lexDigitChar c = let i = fromEnum c in if
    | 0x00030 <= i && i <= 0x00039 -> Just do i - 0x00030
    | 0x00660 <= i && i <= 0x00669 -> Just do i - 0x00660
    | 0x006F0 <= i && i <= 0x006F9 -> Just do i - 0x006F0
    | 0x007C0 <= i && i <= 0x007C9 -> Just do i - 0x007C0
    | 0x00966 <= i && i <= 0x0096F -> Just do i - 0x00966
    | 0x009E6 <= i && i <= 0x009EF -> Just do i - 0x009E6
    | 0x00A66 <= i && i <= 0x00A6F -> Just do i - 0x00A66
    | 0x00AE6 <= i && i <= 0x00AEF -> Just do i - 0x00AE6
    | 0x00B66 <= i && i <= 0x00B6F -> Just do i - 0x00B66
    | 0x00BE6 <= i && i <= 0x00BEF -> Just do i - 0x00BE6
    | 0x00C66 <= i && i <= 0x00C6F -> Just do i - 0x00C66
    | 0x00CE6 <= i && i <= 0x00CEF -> Just do i - 0x00CE6
    | 0x00D66 <= i && i <= 0x00D6F -> Just do i - 0x00D66
    | 0x00DE6 <= i && i <= 0x00DEF -> Just do i - 0x00DE6
    | 0x00E50 <= i && i <= 0x00E59 -> Just do i - 0x00E50
    | 0x00ED0 <= i && i <= 0x00ED9 -> Just do i - 0x00ED0
    | 0x00F20 <= i && i <= 0x00F29 -> Just do i - 0x00F20
    | 0x01040 <= i && i <= 0x01049 -> Just do i - 0x01040
    | 0x01090 <= i && i <= 0x01099 -> Just do i - 0x01090
    | 0x017E0 <= i && i <= 0x017E9 -> Just do i - 0x017E0
    | 0x01810 <= i && i <= 0x01819 -> Just do i - 0x01810
    | 0x01946 <= i && i <= 0x0194F -> Just do i - 0x01946
    | 0x019D0 <= i && i <= 0x019D9 -> Just do i - 0x019D0
    | 0x01A80 <= i && i <= 0x01A89 -> Just do i - 0x01A80
    | 0x01A90 <= i && i <= 0x01A99 -> Just do i - 0x01A90
    | 0x01B50 <= i && i <= 0x01B59 -> Just do i - 0x01B50
    | 0x01BB0 <= i && i <= 0x01BB9 -> Just do i - 0x01BB0
    | 0x01C40 <= i && i <= 0x01C49 -> Just do i - 0x01C40
    | 0x01C50 <= i && i <= 0x01C59 -> Just do i - 0x01C50
    | 0x0A620 <= i && i <= 0x0A629 -> Just do i - 0x0A620
    | 0x0A8D0 <= i && i <= 0x0A8D9 -> Just do i - 0x0A8D0
    | 0x0A900 <= i && i <= 0x0A909 -> Just do i - 0x0A900
    | 0x0A9D0 <= i && i <= 0x0A9D9 -> Just do i - 0x0A9D0
    | 0x0A9F0 <= i && i <= 0x0A9F9 -> Just do i - 0x0A9F0
    | 0x0AA50 <= i && i <= 0x0AA59 -> Just do i - 0x0AA50
    | 0x0ABF0 <= i && i <= 0x0ABF9 -> Just do i - 0x0ABF0
    | 0x0FF10 <= i && i <= 0x0FF19 -> Just do i - 0x0FF10
    | 0x104A0 <= i && i <= 0x104A9 -> Just do i - 0x104A0
    | 0x10D30 <= i && i <= 0x10D39 -> Just do i - 0x10D30
    | 0x11066 <= i && i <= 0x1106F -> Just do i - 0x11066
    | 0x110F0 <= i && i <= 0x110F9 -> Just do i - 0x110F0
    | 0x11136 <= i && i <= 0x1113F -> Just do i - 0x11136
    | 0x111D0 <= i && i <= 0x111D9 -> Just do i - 0x111D0
    | 0x112F0 <= i && i <= 0x112F9 -> Just do i - 0x112F0
    | 0x11450 <= i && i <= 0x11459 -> Just do i - 0x11450
    | 0x114D0 <= i && i <= 0x114D9 -> Just do i - 0x114D0
    | 0x11650 <= i && i <= 0x11659 -> Just do i - 0x11650
    | 0x116C0 <= i && i <= 0x116C9 -> Just do i - 0x116C0
    | 0x11730 <= i && i <= 0x11739 -> Just do i - 0x11730
    | 0x118E0 <= i && i <= 0x118E9 -> Just do i - 0x118E0
    | 0x11C50 <= i && i <= 0x11C59 -> Just do i - 0x11C50
    | 0x11D50 <= i && i <= 0x11D59 -> Just do i - 0x11D50
    | 0x11DA0 <= i && i <= 0x11DA9 -> Just do i - 0x11DA0
    | 0x16A60 <= i && i <= 0x16A69 -> Just do i - 0x16A60
    | 0x16B50 <= i && i <= 0x16B59 -> Just do i - 0x16B50
    | 0x1D7CE <= i && i <= 0x1D7D7 -> Just do i - 0x1D7CE
    | 0x1D7D8 <= i && i <= 0x1D7E1 -> Just do i - 0x1D7D8
    | 0x1D7E2 <= i && i <= 0x1D7EB -> Just do i - 0x1D7E2
    | 0x1D7EC <= i && i <= 0x1D7F5 -> Just do i - 0x1D7EC
    | 0x1D7F6 <= i && i <= 0x1D7FF -> Just do i - 0x1D7F6
    | 0x1E140 <= i && i <= 0x1E149 -> Just do i - 0x1E140
    | 0x1E2F0 <= i && i <= 0x1E2F9 -> Just do i - 0x1E2F0
    | 0x1E950 <= i && i <= 0x1E959 -> Just do i - 0x1E950
    | otherwise                    -> Nothing

-- | Decode a hexit
--
-- prop> case lexHexitChar x of { Nothing -> True; Just i -> 0 <= i && i < 0x10 }
--
{-# INLINE lexHexitChar #-}
lexHexitChar :: Char -> Maybe Int
lexHexitChar = \case
    'A' -> Just 0xA
    'B' -> Just 0xB
    'C' -> Just 0xC
    'D' -> Just 0xD
    'E' -> Just 0xE
    'F' -> Just 0xF
    'a' -> Just 0xA
    'b' -> Just 0xB
    'c' -> Just 0xC
    'd' -> Just 0xD
    'e' -> Just 0xE
    'f' -> Just 0xF
    c   -> lexDigitChar c
