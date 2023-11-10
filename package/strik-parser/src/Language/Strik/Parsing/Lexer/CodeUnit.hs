module Language.Strik.Parsing.Lexer.CodeUnit where

import           Language.Strik.Prelude

import qualified Data.Char              as Char
import qualified Data.EnumSet           as EnumSet


type T = CodeUnit

data CodeUnit
    = CodeUnitByPoint ByPoint
    | CodeUnitOtherByGroup ByGroup
    | CodeUnitOtherByCat ByCat
    deriving (Eq, Show, Ord)

instance Enum CodeUnit where
    toEnum = \case
        i | i < otherByGroupMin -> CodeUnitByPoint do toEnum i
        i | i < otherByCatMin -> CodeUnitOtherByGroup do toEnum do i - otherByGroupMin
        i -> CodeUnitOtherByCat do toEnum do i - otherByCatMin
        where
            byPointMin = 0 :: Int
            otherByGroupMin = byPointMin + 1 + fromEnum do maxBound :: ByPoint
            otherByCatMin = otherByGroupMin + 1 + fromEnum do maxBound :: ByGroup

    fromEnum = \case
        CodeUnitByPoint u -> fromEnum u + byPointMin
        CodeUnitOtherByGroup u -> fromEnum u + otherByGroupMin
        CodeUnitOtherByCat u -> fromEnum u + otherByCatMin
        where
            byPointMin = 0 :: Int
            otherByGroupMin = byPointMin + 1 + fromEnum do maxBound :: ByPoint
            otherByCatMin = otherByGroupMin + 1 + fromEnum do maxBound :: ByGroup

instance Bounded CodeUnit where
    minBound = CodeUnitByPoint minBound
    maxBound = CodeUnitOtherByCat maxBound

data ByPoint
    -- https://www.compart.com/en/unicode/
    = LcU000009 -- '\t'
    | LcU00000A -- '\n'
    | LcU00000B -- '\v'
    | LcU00000C -- '\f'
    | LcU00000D -- '\r'
    | LcU000020 -- ' '
    | LcU000022 -- '"'
    | LcU000023 -- '#'
    | LcU000027 -- '\''
    | LcU000028 -- '('
    | LcU000029 -- ')'
    | LcU00002A -- '*'
    | LcU00002B -- '+'
    | LcU00002C -- ','
    | LcU00002D -- '-'
    | LcU00002E -- '.'
    | LcU00002F -- '/'
    | LcU000030 -- '0'
    | LcU00003A -- ':'
    | LcU00003B -- ';'
    | LcU00003C -- '<'
    | LcU00003D -- '='
    | LcU00003E -- '>'
    | LcU000040 -- '@'
    | LcU000041 -- 'A'
    | LcU000042 -- 'B'
    | LcU000043 -- 'C'
    | LcU000044 -- 'D'
    | LcU000045 -- 'E'
    | LcU000046 -- 'F'
    | LcU000047 -- 'G'
    | LcU000048 -- 'H'
    | LcU000049 -- 'I'
    | LcU00004A -- 'J'
    | LcU00004B -- 'K'
    | LcU00004C -- 'L'
    | LcU00004D -- 'M'
    | LcU00004E -- 'N'
    | LcU00004F -- 'O'
    | LcU000050 -- 'P'
    | LcU000051 -- 'Q'
    | LcU000052 -- 'R'
    | LcU000053 -- 'S'
    | LcU000054 -- 'T'
    | LcU000055 -- 'U'
    | LcU000056 -- 'V'
    | LcU000057 -- 'W'
    | LcU000058 -- 'X'
    | LcU000059 -- 'Y'
    | LcU00005A -- 'Z'
    | LcU00005B -- '['
    | LcU00005C -- '\\'
    | LcU00005D -- ']'
    | LcU00005E -- '^'
    | LcU00005F -- '_'
    | LcU000061 -- 'a'
    | LcU000062 -- 'b'
    | LcU000063 -- 'c'
    | LcU000064 -- 'd'
    | LcU000065 -- 'e'
    | LcU000066 -- 'f'
    | LcU000067 -- 'g'
    | LcU000068 -- 'h'
    | LcU000069 -- 'i'
    | LcU00006A -- 'j'
    | LcU00006B -- 'k'
    | LcU00006C -- 'l'
    | LcU00006D -- 'm'
    | LcU00006E -- 'n'
    | LcU00006F -- 'o'
    | LcU000070 -- 'p'
    | LcU000071 -- 'q'
    | LcU000072 -- 'r'
    | LcU000073 -- 's'
    | LcU000074 -- 't'
    | LcU000075 -- 'u'
    | LcU000076 -- 'v'
    | LcU000077 -- 'w'
    | LcU000078 -- 'x'
    | LcU000079 -- 'y'
    | LcU00007A -- 'z'
    | LcU00007B -- '{'
    | LcU00007D -- '}'
    | LcU00007E -- '~'
    | LcU0003BB -- 'λ'
    | LcU00200E -- Left-to-Right Mark
    | LcU00200F -- Right-to-Left Mark
    | LcU002026 -- '…'
    | LcU002021 -- '‡'
    | LcU002200 -- '∀'
    | LcU002774 -- '❴'
    | LcU002775 -- '❵'
    | LcU002983 -- '⦃'
    | LcU002984 -- '⦄'
    | LcU0029CF -- '⧏'
    | LcU0029D0 -- '⧐'
    | LcU00FE5F -- '﹟'
    deriving (Eq, Ord, Show, Enum, Bounded)

data ByGroup
    -- https://www.compart.com/en/unicode/category/Nd
    = LcGNdDigit0
    | LcGNdDigit1
    | LcGNdDigit2
    | LcGNdDigit3
    | LcGNdDigit4
    | LcGNdDigit5
    | LcGNdDigit6
    | LcGNdDigit7
    | LcGNdDigit8
    | LcGNdDigit9
    deriving (Eq, Ord, Show, Enum, Bounded)

data ByCat
    -- https://www.compart.com/en/unicode/category
    = LcCatCf -- Format
    | LcCatLl -- Lowercase Letter
    | LcCatLm -- Modifier Letter
    | LcCatLo -- Other Letter
    | LcCatLt -- Titlecase Letter
    | LcCatLu -- Uppercase Letter
    | LcCatM  -- Mark
    | LcCatNd -- Decimal Number
    | LcCatNl -- Letter Number
    | LcCatNo -- Other Number
    | LcCatPc -- Connector Punctuation
    | LcCatPd -- Dash Punctuation
    | LcCatPe -- Close Punctuation
    | LcCatPf -- Final Punctuation
    | LcCatPi -- Initial Punctuation
    | LcCatPo -- Other Punctuation
    | LcCatPs -- Open Punctuation
    | LcCatS  -- Symbol
    | LcCatZl -- Line Separator
    | LcCatZp -- Paragraph Separator
    | LcCatZs -- Space Separator

    | LcOther
    deriving (Eq, Ord, Enum, Bounded, Show)

fromChar :: Char -> CodeUnit
fromChar c = case fromEnum c of
    0x000009 -> CodeUnitByPoint LcU000009
    0x00000A -> CodeUnitByPoint LcU00000A
    0x00000B -> CodeUnitByPoint LcU00000B
    0x00000C -> CodeUnitByPoint LcU00000C
    0x00000D -> CodeUnitByPoint LcU00000D
    0x000020 -> CodeUnitByPoint LcU000020
    0x000022 -> CodeUnitByPoint LcU000022
    0x000023 -> CodeUnitByPoint LcU000023
    0x000027 -> CodeUnitByPoint LcU000027
    0x000028 -> CodeUnitByPoint LcU000028
    0x000029 -> CodeUnitByPoint LcU000029
    0x00002A -> CodeUnitByPoint LcU00002A
    0x00002B -> CodeUnitByPoint LcU00002B
    0x00002C -> CodeUnitByPoint LcU00002C
    0x00002D -> CodeUnitByPoint LcU00002D
    0x00002E -> CodeUnitByPoint LcU00002E
    0x00002F -> CodeUnitByPoint LcU00002F
    0x000030 -> CodeUnitByPoint LcU000030
    0x000031 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000032 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000033 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000034 -> CodeUnitOtherByGroup LcGNdDigit4
    0x000035 -> CodeUnitOtherByGroup LcGNdDigit5
    0x000036 -> CodeUnitOtherByGroup LcGNdDigit6
    0x000037 -> CodeUnitOtherByGroup LcGNdDigit7
    0x000038 -> CodeUnitOtherByGroup LcGNdDigit8
    0x000039 -> CodeUnitOtherByGroup LcGNdDigit9
    0x00003A -> CodeUnitByPoint LcU00003A
    0x00003B -> CodeUnitByPoint LcU00003B
    0x00003C -> CodeUnitByPoint LcU00003C
    0x00003D -> CodeUnitByPoint LcU00003D
    0x00003E -> CodeUnitByPoint LcU00003E
    0x000040 -> CodeUnitByPoint LcU000040
    0x000041 -> CodeUnitByPoint LcU000041
    0x000042 -> CodeUnitByPoint LcU000042
    0x000043 -> CodeUnitByPoint LcU000043
    0x000044 -> CodeUnitByPoint LcU000044
    0x000045 -> CodeUnitByPoint LcU000045
    0x000046 -> CodeUnitByPoint LcU000046
    0x000047 -> CodeUnitByPoint LcU000047
    0x000048 -> CodeUnitByPoint LcU000048
    0x000049 -> CodeUnitByPoint LcU000049
    0x00004A -> CodeUnitByPoint LcU00004A
    0x00004B -> CodeUnitByPoint LcU00004B
    0x00004C -> CodeUnitByPoint LcU00004C
    0x00004D -> CodeUnitByPoint LcU00004D
    0x00004E -> CodeUnitByPoint LcU00004E
    0x00004F -> CodeUnitByPoint LcU00004F
    0x000050 -> CodeUnitByPoint LcU000050
    0x000051 -> CodeUnitByPoint LcU000051
    0x000052 -> CodeUnitByPoint LcU000052
    0x000053 -> CodeUnitByPoint LcU000053
    0x000054 -> CodeUnitByPoint LcU000054
    0x000055 -> CodeUnitByPoint LcU000055
    0x000056 -> CodeUnitByPoint LcU000056
    0x000057 -> CodeUnitByPoint LcU000057
    0x000058 -> CodeUnitByPoint LcU000058
    0x000059 -> CodeUnitByPoint LcU000059
    0x00005A -> CodeUnitByPoint LcU00005A
    0x00005B -> CodeUnitByPoint LcU00005B
    0x00005C -> CodeUnitByPoint LcU00005C
    0x00005D -> CodeUnitByPoint LcU00005D
    0x00005E -> CodeUnitByPoint LcU00005E
    0x00005F -> CodeUnitByPoint LcU00005F
    0x000061 -> CodeUnitByPoint LcU000061
    0x000062 -> CodeUnitByPoint LcU000062
    0x000063 -> CodeUnitByPoint LcU000063
    0x000064 -> CodeUnitByPoint LcU000064
    0x000065 -> CodeUnitByPoint LcU000065
    0x000066 -> CodeUnitByPoint LcU000066
    0x000067 -> CodeUnitByPoint LcU000067
    0x000068 -> CodeUnitByPoint LcU000068
    0x000069 -> CodeUnitByPoint LcU000069
    0x00006A -> CodeUnitByPoint LcU00006A
    0x00006B -> CodeUnitByPoint LcU00006B
    0x00006C -> CodeUnitByPoint LcU00006C
    0x00006D -> CodeUnitByPoint LcU00006D
    0x00006E -> CodeUnitByPoint LcU00006E
    0x00006F -> CodeUnitByPoint LcU00006F
    0x000070 -> CodeUnitByPoint LcU000070
    0x000071 -> CodeUnitByPoint LcU000071
    0x000072 -> CodeUnitByPoint LcU000072
    0x000073 -> CodeUnitByPoint LcU000073
    0x000074 -> CodeUnitByPoint LcU000074
    0x000075 -> CodeUnitByPoint LcU000075
    0x000076 -> CodeUnitByPoint LcU000076
    0x000077 -> CodeUnitByPoint LcU000077
    0x000078 -> CodeUnitByPoint LcU000078
    0x000079 -> CodeUnitByPoint LcU000079
    0x00007A -> CodeUnitByPoint LcU00007A
    0x00007B -> CodeUnitByPoint LcU00007B
    0x00007D -> CodeUnitByPoint LcU00007D
    0x00007E -> CodeUnitByPoint LcU00007E
    0x0003BB -> CodeUnitByPoint LcU0003BB
    0x000660 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000661 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000662 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000663 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000664 -> CodeUnitOtherByGroup LcGNdDigit4
    0x000665 -> CodeUnitOtherByGroup LcGNdDigit5
    0x000666 -> CodeUnitOtherByGroup LcGNdDigit6
    0x000667 -> CodeUnitOtherByGroup LcGNdDigit7
    0x000668 -> CodeUnitOtherByGroup LcGNdDigit8
    0x000669 -> CodeUnitOtherByGroup LcGNdDigit9
    0x0006F0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0006F1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0006F2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0006F3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0006F4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0006F5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0006F6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0006F7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0006F8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0006F9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x0007C0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0007C1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0007C2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0007C3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0007C4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0007C5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0007C6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0007C7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0007C8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0007C9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x000966 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000967 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000968 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000969 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00096A -> CodeUnitOtherByGroup LcGNdDigit4
    0x00096B -> CodeUnitOtherByGroup LcGNdDigit5
    0x00096C -> CodeUnitOtherByGroup LcGNdDigit6
    0x00096D -> CodeUnitOtherByGroup LcGNdDigit7
    0x00096E -> CodeUnitOtherByGroup LcGNdDigit8
    0x00096F -> CodeUnitOtherByGroup LcGNdDigit9
    0x0009E6 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0009E7 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0009E8 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0009E9 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0009EA -> CodeUnitOtherByGroup LcGNdDigit4
    0x0009EB -> CodeUnitOtherByGroup LcGNdDigit5
    0x0009EC -> CodeUnitOtherByGroup LcGNdDigit6
    0x0009ED -> CodeUnitOtherByGroup LcGNdDigit7
    0x0009EE -> CodeUnitOtherByGroup LcGNdDigit8
    0x0009EF -> CodeUnitOtherByGroup LcGNdDigit9
    0x000A66 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000A67 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000A68 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000A69 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000A6A -> CodeUnitOtherByGroup LcGNdDigit4
    0x000A6B -> CodeUnitOtherByGroup LcGNdDigit5
    0x000A6C -> CodeUnitOtherByGroup LcGNdDigit6
    0x000A6D -> CodeUnitOtherByGroup LcGNdDigit7
    0x000A6E -> CodeUnitOtherByGroup LcGNdDigit8
    0x000A6F -> CodeUnitOtherByGroup LcGNdDigit9
    0x000AE6 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000AE7 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000AE8 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000AE9 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000AEA -> CodeUnitOtherByGroup LcGNdDigit4
    0x000AEB -> CodeUnitOtherByGroup LcGNdDigit5
    0x000AEC -> CodeUnitOtherByGroup LcGNdDigit6
    0x000AED -> CodeUnitOtherByGroup LcGNdDigit7
    0x000AEE -> CodeUnitOtherByGroup LcGNdDigit8
    0x000AEF -> CodeUnitOtherByGroup LcGNdDigit9
    0x000B66 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000B67 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000B68 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000B69 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000B6A -> CodeUnitOtherByGroup LcGNdDigit4
    0x000B6B -> CodeUnitOtherByGroup LcGNdDigit5
    0x000B6C -> CodeUnitOtherByGroup LcGNdDigit6
    0x000B6D -> CodeUnitOtherByGroup LcGNdDigit7
    0x000B6E -> CodeUnitOtherByGroup LcGNdDigit8
    0x000B6F -> CodeUnitOtherByGroup LcGNdDigit9
    0x000BE6 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000BE7 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000BE8 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000BE9 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000BEA -> CodeUnitOtherByGroup LcGNdDigit4
    0x000BEB -> CodeUnitOtherByGroup LcGNdDigit5
    0x000BEC -> CodeUnitOtherByGroup LcGNdDigit6
    0x000BED -> CodeUnitOtherByGroup LcGNdDigit7
    0x000BEE -> CodeUnitOtherByGroup LcGNdDigit8
    0x000BEF -> CodeUnitOtherByGroup LcGNdDigit9
    0x000C66 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000C67 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000C68 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000C69 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000C6A -> CodeUnitOtherByGroup LcGNdDigit4
    0x000C6B -> CodeUnitOtherByGroup LcGNdDigit5
    0x000C6C -> CodeUnitOtherByGroup LcGNdDigit6
    0x000C6D -> CodeUnitOtherByGroup LcGNdDigit7
    0x000C6E -> CodeUnitOtherByGroup LcGNdDigit8
    0x000C6F -> CodeUnitOtherByGroup LcGNdDigit9
    0x000CE6 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000CE7 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000CE8 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000CE9 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000CEA -> CodeUnitOtherByGroup LcGNdDigit4
    0x000CEB -> CodeUnitOtherByGroup LcGNdDigit5
    0x000CEC -> CodeUnitOtherByGroup LcGNdDigit6
    0x000CED -> CodeUnitOtherByGroup LcGNdDigit7
    0x000CEE -> CodeUnitOtherByGroup LcGNdDigit8
    0x000CEF -> CodeUnitOtherByGroup LcGNdDigit9
    0x000D66 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000D67 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000D68 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000D69 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000D6A -> CodeUnitOtherByGroup LcGNdDigit4
    0x000D6B -> CodeUnitOtherByGroup LcGNdDigit5
    0x000D6C -> CodeUnitOtherByGroup LcGNdDigit6
    0x000D6D -> CodeUnitOtherByGroup LcGNdDigit7
    0x000D6E -> CodeUnitOtherByGroup LcGNdDigit8
    0x000D6F -> CodeUnitOtherByGroup LcGNdDigit9
    0x000DE6 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000DE7 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000DE8 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000DE9 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000DEA -> CodeUnitOtherByGroup LcGNdDigit4
    0x000DEB -> CodeUnitOtherByGroup LcGNdDigit5
    0x000DEC -> CodeUnitOtherByGroup LcGNdDigit6
    0x000DED -> CodeUnitOtherByGroup LcGNdDigit7
    0x000DEE -> CodeUnitOtherByGroup LcGNdDigit8
    0x000DEF -> CodeUnitOtherByGroup LcGNdDigit9
    0x000E50 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000E51 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000E52 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000E53 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000E54 -> CodeUnitOtherByGroup LcGNdDigit4
    0x000E55 -> CodeUnitOtherByGroup LcGNdDigit5
    0x000E56 -> CodeUnitOtherByGroup LcGNdDigit6
    0x000E57 -> CodeUnitOtherByGroup LcGNdDigit7
    0x000E58 -> CodeUnitOtherByGroup LcGNdDigit8
    0x000E59 -> CodeUnitOtherByGroup LcGNdDigit9
    0x000ED0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000ED1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000ED2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000ED3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000ED4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x000ED5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x000ED6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x000ED7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x000ED8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x000ED9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x000F20 -> CodeUnitOtherByGroup LcGNdDigit0
    0x000F21 -> CodeUnitOtherByGroup LcGNdDigit1
    0x000F22 -> CodeUnitOtherByGroup LcGNdDigit2
    0x000F23 -> CodeUnitOtherByGroup LcGNdDigit3
    0x000F24 -> CodeUnitOtherByGroup LcGNdDigit4
    0x000F25 -> CodeUnitOtherByGroup LcGNdDigit5
    0x000F26 -> CodeUnitOtherByGroup LcGNdDigit6
    0x000F27 -> CodeUnitOtherByGroup LcGNdDigit7
    0x000F28 -> CodeUnitOtherByGroup LcGNdDigit8
    0x000F29 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001040 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001041 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001042 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001043 -> CodeUnitOtherByGroup LcGNdDigit3
    0x001044 -> CodeUnitOtherByGroup LcGNdDigit4
    0x001045 -> CodeUnitOtherByGroup LcGNdDigit5
    0x001046 -> CodeUnitOtherByGroup LcGNdDigit6
    0x001047 -> CodeUnitOtherByGroup LcGNdDigit7
    0x001048 -> CodeUnitOtherByGroup LcGNdDigit8
    0x001049 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001090 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001091 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001092 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001093 -> CodeUnitOtherByGroup LcGNdDigit3
    0x001094 -> CodeUnitOtherByGroup LcGNdDigit4
    0x001095 -> CodeUnitOtherByGroup LcGNdDigit5
    0x001096 -> CodeUnitOtherByGroup LcGNdDigit6
    0x001097 -> CodeUnitOtherByGroup LcGNdDigit7
    0x001098 -> CodeUnitOtherByGroup LcGNdDigit8
    0x001099 -> CodeUnitOtherByGroup LcGNdDigit9
    0x0017E0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0017E1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0017E2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0017E3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0017E4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0017E5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0017E6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0017E7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0017E8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0017E9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001810 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001811 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001812 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001813 -> CodeUnitOtherByGroup LcGNdDigit3
    0x001814 -> CodeUnitOtherByGroup LcGNdDigit4
    0x001815 -> CodeUnitOtherByGroup LcGNdDigit5
    0x001816 -> CodeUnitOtherByGroup LcGNdDigit6
    0x001817 -> CodeUnitOtherByGroup LcGNdDigit7
    0x001818 -> CodeUnitOtherByGroup LcGNdDigit8
    0x001819 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001946 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001947 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001948 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001949 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00194A -> CodeUnitOtherByGroup LcGNdDigit4
    0x00194B -> CodeUnitOtherByGroup LcGNdDigit5
    0x00194C -> CodeUnitOtherByGroup LcGNdDigit6
    0x00194D -> CodeUnitOtherByGroup LcGNdDigit7
    0x00194E -> CodeUnitOtherByGroup LcGNdDigit8
    0x00194F -> CodeUnitOtherByGroup LcGNdDigit9
    0x0019D0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0019D1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0019D2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0019D3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0019D4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0019D5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0019D6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0019D7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0019D8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0019D9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001A80 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001A81 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001A82 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001A83 -> CodeUnitOtherByGroup LcGNdDigit3
    0x001A84 -> CodeUnitOtherByGroup LcGNdDigit4
    0x001A85 -> CodeUnitOtherByGroup LcGNdDigit5
    0x001A86 -> CodeUnitOtherByGroup LcGNdDigit6
    0x001A87 -> CodeUnitOtherByGroup LcGNdDigit7
    0x001A88 -> CodeUnitOtherByGroup LcGNdDigit8
    0x001A89 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001A90 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001A91 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001A92 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001A93 -> CodeUnitOtherByGroup LcGNdDigit3
    0x001A94 -> CodeUnitOtherByGroup LcGNdDigit4
    0x001A95 -> CodeUnitOtherByGroup LcGNdDigit5
    0x001A96 -> CodeUnitOtherByGroup LcGNdDigit6
    0x001A97 -> CodeUnitOtherByGroup LcGNdDigit7
    0x001A98 -> CodeUnitOtherByGroup LcGNdDigit8
    0x001A99 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001B50 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001B51 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001B52 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001B53 -> CodeUnitOtherByGroup LcGNdDigit3
    0x001B54 -> CodeUnitOtherByGroup LcGNdDigit4
    0x001B55 -> CodeUnitOtherByGroup LcGNdDigit5
    0x001B56 -> CodeUnitOtherByGroup LcGNdDigit6
    0x001B57 -> CodeUnitOtherByGroup LcGNdDigit7
    0x001B58 -> CodeUnitOtherByGroup LcGNdDigit8
    0x001B59 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001BB0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001BB1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001BB2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001BB3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x001BB4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x001BB5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x001BB6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x001BB7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x001BB8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x001BB9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001C40 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001C41 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001C42 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001C43 -> CodeUnitOtherByGroup LcGNdDigit3
    0x001C44 -> CodeUnitOtherByGroup LcGNdDigit4
    0x001C45 -> CodeUnitOtherByGroup LcGNdDigit5
    0x001C46 -> CodeUnitOtherByGroup LcGNdDigit6
    0x001C47 -> CodeUnitOtherByGroup LcGNdDigit7
    0x001C48 -> CodeUnitOtherByGroup LcGNdDigit8
    0x001C49 -> CodeUnitOtherByGroup LcGNdDigit9
    0x001C50 -> CodeUnitOtherByGroup LcGNdDigit0
    0x001C51 -> CodeUnitOtherByGroup LcGNdDigit1
    0x001C52 -> CodeUnitOtherByGroup LcGNdDigit2
    0x001C53 -> CodeUnitOtherByGroup LcGNdDigit3
    0x001C54 -> CodeUnitOtherByGroup LcGNdDigit4
    0x001C55 -> CodeUnitOtherByGroup LcGNdDigit5
    0x001C56 -> CodeUnitOtherByGroup LcGNdDigit6
    0x001C57 -> CodeUnitOtherByGroup LcGNdDigit7
    0x001C58 -> CodeUnitOtherByGroup LcGNdDigit8
    0x001C59 -> CodeUnitOtherByGroup LcGNdDigit9
    0x00200E -> CodeUnitByPoint LcU00200E
    0x00200F -> CodeUnitByPoint LcU00200F
    0x002021 -> CodeUnitByPoint LcU002021
    0x002026 -> CodeUnitByPoint LcU002026
    0x002200 -> CodeUnitByPoint LcU002200
    0x002774 -> CodeUnitByPoint LcU002774
    0x002775 -> CodeUnitByPoint LcU002775
    0x002983 -> CodeUnitByPoint LcU002983
    0x002984 -> CodeUnitByPoint LcU002984
    0x0029CF -> CodeUnitByPoint LcU0029CF
    0x0029D0 -> CodeUnitByPoint LcU0029D0
    0x00A620 -> CodeUnitOtherByGroup LcGNdDigit0
    0x00A621 -> CodeUnitOtherByGroup LcGNdDigit1
    0x00A622 -> CodeUnitOtherByGroup LcGNdDigit2
    0x00A623 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00A624 -> CodeUnitOtherByGroup LcGNdDigit4
    0x00A625 -> CodeUnitOtherByGroup LcGNdDigit5
    0x00A626 -> CodeUnitOtherByGroup LcGNdDigit6
    0x00A627 -> CodeUnitOtherByGroup LcGNdDigit7
    0x00A628 -> CodeUnitOtherByGroup LcGNdDigit8
    0x00A629 -> CodeUnitOtherByGroup LcGNdDigit9
    0x00A8D0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x00A8D1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x00A8D2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x00A8D3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00A8D4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x00A8D5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x00A8D6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x00A8D7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x00A8D8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x00A8D9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x00A900 -> CodeUnitOtherByGroup LcGNdDigit0
    0x00A901 -> CodeUnitOtherByGroup LcGNdDigit1
    0x00A902 -> CodeUnitOtherByGroup LcGNdDigit2
    0x00A903 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00A904 -> CodeUnitOtherByGroup LcGNdDigit4
    0x00A905 -> CodeUnitOtherByGroup LcGNdDigit5
    0x00A906 -> CodeUnitOtherByGroup LcGNdDigit6
    0x00A907 -> CodeUnitOtherByGroup LcGNdDigit7
    0x00A908 -> CodeUnitOtherByGroup LcGNdDigit8
    0x00A909 -> CodeUnitOtherByGroup LcGNdDigit9
    0x00A9D0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x00A9D1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x00A9D2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x00A9D3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00A9D4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x00A9D5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x00A9D6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x00A9D7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x00A9D8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x00A9D9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x00A9F0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x00A9F1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x00A9F2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x00A9F3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00A9F4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x00A9F5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x00A9F6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x00A9F7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x00A9F8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x00A9F9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x00AA50 -> CodeUnitOtherByGroup LcGNdDigit0
    0x00AA51 -> CodeUnitOtherByGroup LcGNdDigit1
    0x00AA52 -> CodeUnitOtherByGroup LcGNdDigit2
    0x00AA53 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00AA54 -> CodeUnitOtherByGroup LcGNdDigit4
    0x00AA55 -> CodeUnitOtherByGroup LcGNdDigit5
    0x00AA56 -> CodeUnitOtherByGroup LcGNdDigit6
    0x00AA57 -> CodeUnitOtherByGroup LcGNdDigit7
    0x00AA58 -> CodeUnitOtherByGroup LcGNdDigit8
    0x00AA59 -> CodeUnitOtherByGroup LcGNdDigit9
    0x00ABF0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x00ABF1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x00ABF2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x00ABF3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00ABF4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x00ABF5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x00ABF6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x00ABF7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x00ABF8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x00ABF9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x00FE5F -> CodeUnitByPoint LcU00FE5F
    0x00FF10 -> CodeUnitOtherByGroup LcGNdDigit0
    0x00FF11 -> CodeUnitOtherByGroup LcGNdDigit1
    0x00FF12 -> CodeUnitOtherByGroup LcGNdDigit2
    0x00FF13 -> CodeUnitOtherByGroup LcGNdDigit3
    0x00FF14 -> CodeUnitOtherByGroup LcGNdDigit4
    0x00FF15 -> CodeUnitOtherByGroup LcGNdDigit5
    0x00FF16 -> CodeUnitOtherByGroup LcGNdDigit6
    0x00FF17 -> CodeUnitOtherByGroup LcGNdDigit7
    0x00FF18 -> CodeUnitOtherByGroup LcGNdDigit8
    0x00FF19 -> CodeUnitOtherByGroup LcGNdDigit9
    0x0104A0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0104A1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0104A2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0104A3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0104A4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0104A5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0104A6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0104A7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0104A8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0104A9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x010D30 -> CodeUnitOtherByGroup LcGNdDigit0
    0x010D31 -> CodeUnitOtherByGroup LcGNdDigit1
    0x010D32 -> CodeUnitOtherByGroup LcGNdDigit2
    0x010D33 -> CodeUnitOtherByGroup LcGNdDigit3
    0x010D34 -> CodeUnitOtherByGroup LcGNdDigit4
    0x010D35 -> CodeUnitOtherByGroup LcGNdDigit5
    0x010D36 -> CodeUnitOtherByGroup LcGNdDigit6
    0x010D37 -> CodeUnitOtherByGroup LcGNdDigit7
    0x010D38 -> CodeUnitOtherByGroup LcGNdDigit8
    0x010D39 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011066 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011067 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011068 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011069 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01106A -> CodeUnitOtherByGroup LcGNdDigit4
    0x01106B -> CodeUnitOtherByGroup LcGNdDigit5
    0x01106C -> CodeUnitOtherByGroup LcGNdDigit6
    0x01106D -> CodeUnitOtherByGroup LcGNdDigit7
    0x01106E -> CodeUnitOtherByGroup LcGNdDigit8
    0x01106F -> CodeUnitOtherByGroup LcGNdDigit9
    0x0110F0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0110F1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0110F2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0110F3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0110F4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0110F5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0110F6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0110F7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0110F8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0110F9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011136 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011137 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011138 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011139 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01113A -> CodeUnitOtherByGroup LcGNdDigit4
    0x01113B -> CodeUnitOtherByGroup LcGNdDigit5
    0x01113C -> CodeUnitOtherByGroup LcGNdDigit6
    0x01113D -> CodeUnitOtherByGroup LcGNdDigit7
    0x01113E -> CodeUnitOtherByGroup LcGNdDigit8
    0x01113F -> CodeUnitOtherByGroup LcGNdDigit9
    0x0111D0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0111D1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0111D2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0111D3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0111D4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0111D5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0111D6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0111D7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0111D8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0111D9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x0112F0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0112F1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0112F2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0112F3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0112F4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0112F5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0112F6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0112F7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0112F8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0112F9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011450 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011451 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011452 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011453 -> CodeUnitOtherByGroup LcGNdDigit3
    0x011454 -> CodeUnitOtherByGroup LcGNdDigit4
    0x011455 -> CodeUnitOtherByGroup LcGNdDigit5
    0x011456 -> CodeUnitOtherByGroup LcGNdDigit6
    0x011457 -> CodeUnitOtherByGroup LcGNdDigit7
    0x011458 -> CodeUnitOtherByGroup LcGNdDigit8
    0x011459 -> CodeUnitOtherByGroup LcGNdDigit9
    0x0114D0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0114D1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0114D2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0114D3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0114D4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0114D5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0114D6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0114D7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0114D8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0114D9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011650 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011651 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011652 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011653 -> CodeUnitOtherByGroup LcGNdDigit3
    0x011654 -> CodeUnitOtherByGroup LcGNdDigit4
    0x011655 -> CodeUnitOtherByGroup LcGNdDigit5
    0x011656 -> CodeUnitOtherByGroup LcGNdDigit6
    0x011657 -> CodeUnitOtherByGroup LcGNdDigit7
    0x011658 -> CodeUnitOtherByGroup LcGNdDigit8
    0x011659 -> CodeUnitOtherByGroup LcGNdDigit9
    0x0116C0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0116C1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0116C2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0116C3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0116C4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0116C5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0116C6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0116C7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0116C8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0116C9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011730 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011731 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011732 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011733 -> CodeUnitOtherByGroup LcGNdDigit3
    0x011734 -> CodeUnitOtherByGroup LcGNdDigit4
    0x011735 -> CodeUnitOtherByGroup LcGNdDigit5
    0x011736 -> CodeUnitOtherByGroup LcGNdDigit6
    0x011737 -> CodeUnitOtherByGroup LcGNdDigit7
    0x011738 -> CodeUnitOtherByGroup LcGNdDigit8
    0x011739 -> CodeUnitOtherByGroup LcGNdDigit9
    0x0118E0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x0118E1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x0118E2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x0118E3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x0118E4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x0118E5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x0118E6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x0118E7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x0118E8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x0118E9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011950 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011951 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011952 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011953 -> CodeUnitOtherByGroup LcGNdDigit3
    0x011954 -> CodeUnitOtherByGroup LcGNdDigit4
    0x011955 -> CodeUnitOtherByGroup LcGNdDigit5
    0x011956 -> CodeUnitOtherByGroup LcGNdDigit6
    0x011957 -> CodeUnitOtherByGroup LcGNdDigit7
    0x011958 -> CodeUnitOtherByGroup LcGNdDigit8
    0x011959 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011C50 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011C51 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011C52 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011C53 -> CodeUnitOtherByGroup LcGNdDigit3
    0x011C54 -> CodeUnitOtherByGroup LcGNdDigit4
    0x011C55 -> CodeUnitOtherByGroup LcGNdDigit5
    0x011C56 -> CodeUnitOtherByGroup LcGNdDigit6
    0x011C57 -> CodeUnitOtherByGroup LcGNdDigit7
    0x011C58 -> CodeUnitOtherByGroup LcGNdDigit8
    0x011C59 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011D50 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011D51 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011D52 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011D53 -> CodeUnitOtherByGroup LcGNdDigit3
    0x011D54 -> CodeUnitOtherByGroup LcGNdDigit4
    0x011D55 -> CodeUnitOtherByGroup LcGNdDigit5
    0x011D56 -> CodeUnitOtherByGroup LcGNdDigit6
    0x011D57 -> CodeUnitOtherByGroup LcGNdDigit7
    0x011D58 -> CodeUnitOtherByGroup LcGNdDigit8
    0x011D59 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011DA0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011DA1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011DA2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011DA3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x011DA4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x011DA5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x011DA6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x011DA7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x011DA8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x011DA9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x011F50 -> CodeUnitOtherByGroup LcGNdDigit0
    0x011F51 -> CodeUnitOtherByGroup LcGNdDigit1
    0x011F52 -> CodeUnitOtherByGroup LcGNdDigit2
    0x011F53 -> CodeUnitOtherByGroup LcGNdDigit3
    0x011F54 -> CodeUnitOtherByGroup LcGNdDigit4
    0x011F55 -> CodeUnitOtherByGroup LcGNdDigit5
    0x011F56 -> CodeUnitOtherByGroup LcGNdDigit6
    0x011F57 -> CodeUnitOtherByGroup LcGNdDigit7
    0x011F58 -> CodeUnitOtherByGroup LcGNdDigit8
    0x011F59 -> CodeUnitOtherByGroup LcGNdDigit9
    0x016A60 -> CodeUnitOtherByGroup LcGNdDigit0
    0x016A61 -> CodeUnitOtherByGroup LcGNdDigit1
    0x016A62 -> CodeUnitOtherByGroup LcGNdDigit2
    0x016A63 -> CodeUnitOtherByGroup LcGNdDigit3
    0x016A64 -> CodeUnitOtherByGroup LcGNdDigit4
    0x016A65 -> CodeUnitOtherByGroup LcGNdDigit5
    0x016A66 -> CodeUnitOtherByGroup LcGNdDigit6
    0x016A67 -> CodeUnitOtherByGroup LcGNdDigit7
    0x016A68 -> CodeUnitOtherByGroup LcGNdDigit8
    0x016A69 -> CodeUnitOtherByGroup LcGNdDigit9
    0x016AC0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x016AC1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x016AC2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x016AC3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x016AC4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x016AC5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x016AC6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x016AC7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x016AC8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x016AC9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x016B50 -> CodeUnitOtherByGroup LcGNdDigit0
    0x016B51 -> CodeUnitOtherByGroup LcGNdDigit1
    0x016B52 -> CodeUnitOtherByGroup LcGNdDigit2
    0x016B53 -> CodeUnitOtherByGroup LcGNdDigit3
    0x016B54 -> CodeUnitOtherByGroup LcGNdDigit4
    0x016B55 -> CodeUnitOtherByGroup LcGNdDigit5
    0x016B56 -> CodeUnitOtherByGroup LcGNdDigit6
    0x016B57 -> CodeUnitOtherByGroup LcGNdDigit7
    0x016B58 -> CodeUnitOtherByGroup LcGNdDigit8
    0x016B59 -> CodeUnitOtherByGroup LcGNdDigit9
    0x01D7CE -> CodeUnitOtherByGroup LcGNdDigit0
    0x01D7CF -> CodeUnitOtherByGroup LcGNdDigit1
    0x01D7D0 -> CodeUnitOtherByGroup LcGNdDigit2
    0x01D7D1 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01D7D2 -> CodeUnitOtherByGroup LcGNdDigit4
    0x01D7D3 -> CodeUnitOtherByGroup LcGNdDigit5
    0x01D7D4 -> CodeUnitOtherByGroup LcGNdDigit6
    0x01D7D5 -> CodeUnitOtherByGroup LcGNdDigit7
    0x01D7D6 -> CodeUnitOtherByGroup LcGNdDigit8
    0x01D7D7 -> CodeUnitOtherByGroup LcGNdDigit9
    0x01D7D8 -> CodeUnitOtherByGroup LcGNdDigit0
    0x01D7D9 -> CodeUnitOtherByGroup LcGNdDigit1
    0x01D7DA -> CodeUnitOtherByGroup LcGNdDigit2
    0x01D7DB -> CodeUnitOtherByGroup LcGNdDigit3
    0x01D7DC -> CodeUnitOtherByGroup LcGNdDigit4
    0x01D7DD -> CodeUnitOtherByGroup LcGNdDigit5
    0x01D7DE -> CodeUnitOtherByGroup LcGNdDigit6
    0x01D7DF -> CodeUnitOtherByGroup LcGNdDigit7
    0x01D7E0 -> CodeUnitOtherByGroup LcGNdDigit8
    0x01D7E1 -> CodeUnitOtherByGroup LcGNdDigit9
    0x01D7E2 -> CodeUnitOtherByGroup LcGNdDigit0
    0x01D7E3 -> CodeUnitOtherByGroup LcGNdDigit1
    0x01D7E4 -> CodeUnitOtherByGroup LcGNdDigit2
    0x01D7E5 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01D7E6 -> CodeUnitOtherByGroup LcGNdDigit4
    0x01D7E7 -> CodeUnitOtherByGroup LcGNdDigit5
    0x01D7E8 -> CodeUnitOtherByGroup LcGNdDigit6
    0x01D7E9 -> CodeUnitOtherByGroup LcGNdDigit7
    0x01D7EA -> CodeUnitOtherByGroup LcGNdDigit8
    0x01D7EB -> CodeUnitOtherByGroup LcGNdDigit9
    0x01D7EC -> CodeUnitOtherByGroup LcGNdDigit0
    0x01D7ED -> CodeUnitOtherByGroup LcGNdDigit1
    0x01D7EE -> CodeUnitOtherByGroup LcGNdDigit2
    0x01D7EF -> CodeUnitOtherByGroup LcGNdDigit3
    0x01D7F0 -> CodeUnitOtherByGroup LcGNdDigit4
    0x01D7F1 -> CodeUnitOtherByGroup LcGNdDigit5
    0x01D7F2 -> CodeUnitOtherByGroup LcGNdDigit6
    0x01D7F3 -> CodeUnitOtherByGroup LcGNdDigit7
    0x01D7F4 -> CodeUnitOtherByGroup LcGNdDigit8
    0x01D7F5 -> CodeUnitOtherByGroup LcGNdDigit9
    0x01D7F6 -> CodeUnitOtherByGroup LcGNdDigit0
    0x01D7F7 -> CodeUnitOtherByGroup LcGNdDigit1
    0x01D7F8 -> CodeUnitOtherByGroup LcGNdDigit2
    0x01D7F9 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01D7FA -> CodeUnitOtherByGroup LcGNdDigit4
    0x01D7FB -> CodeUnitOtherByGroup LcGNdDigit5
    0x01D7FC -> CodeUnitOtherByGroup LcGNdDigit6
    0x01D7FD -> CodeUnitOtherByGroup LcGNdDigit7
    0x01D7FE -> CodeUnitOtherByGroup LcGNdDigit8
    0x01D7FF -> CodeUnitOtherByGroup LcGNdDigit9
    0x01E140 -> CodeUnitOtherByGroup LcGNdDigit0
    0x01E141 -> CodeUnitOtherByGroup LcGNdDigit1
    0x01E142 -> CodeUnitOtherByGroup LcGNdDigit2
    0x01E143 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01E144 -> CodeUnitOtherByGroup LcGNdDigit4
    0x01E145 -> CodeUnitOtherByGroup LcGNdDigit5
    0x01E146 -> CodeUnitOtherByGroup LcGNdDigit6
    0x01E147 -> CodeUnitOtherByGroup LcGNdDigit7
    0x01E148 -> CodeUnitOtherByGroup LcGNdDigit8
    0x01E149 -> CodeUnitOtherByGroup LcGNdDigit9
    0x01E2F0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x01E2F1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x01E2F2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x01E2F3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01E2F4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x01E2F5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x01E2F6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x01E2F7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x01E2F8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x01E2F9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x01E4F0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x01E4F1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x01E4F2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x01E4F3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01E4F4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x01E4F5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x01E4F6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x01E4F7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x01E4F8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x01E4F9 -> CodeUnitOtherByGroup LcGNdDigit9
    0x01E950 -> CodeUnitOtherByGroup LcGNdDigit0
    0x01E951 -> CodeUnitOtherByGroup LcGNdDigit1
    0x01E952 -> CodeUnitOtherByGroup LcGNdDigit2
    0x01E953 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01E954 -> CodeUnitOtherByGroup LcGNdDigit4
    0x01E955 -> CodeUnitOtherByGroup LcGNdDigit5
    0x01E956 -> CodeUnitOtherByGroup LcGNdDigit6
    0x01E957 -> CodeUnitOtherByGroup LcGNdDigit7
    0x01E958 -> CodeUnitOtherByGroup LcGNdDigit8
    0x01E959 -> CodeUnitOtherByGroup LcGNdDigit9
    0x01FBF0 -> CodeUnitOtherByGroup LcGNdDigit0
    0x01FBF1 -> CodeUnitOtherByGroup LcGNdDigit1
    0x01FBF2 -> CodeUnitOtherByGroup LcGNdDigit2
    0x01FBF3 -> CodeUnitOtherByGroup LcGNdDigit3
    0x01FBF4 -> CodeUnitOtherByGroup LcGNdDigit4
    0x01FBF5 -> CodeUnitOtherByGroup LcGNdDigit5
    0x01FBF6 -> CodeUnitOtherByGroup LcGNdDigit6
    0x01FBF7 -> CodeUnitOtherByGroup LcGNdDigit7
    0x01FBF8 -> CodeUnitOtherByGroup LcGNdDigit8
    0x01FBF9 -> CodeUnitOtherByGroup LcGNdDigit9
    _      -> CodeUnitOtherByCat case Char.generalCategory c of
        Char.Format               -> LcCatCf
        Char.LowercaseLetter      -> LcCatLl
        Char.ModifierLetter       -> LcCatLm
        Char.OtherLetter          -> LcCatLo
        Char.TitlecaseLetter      -> LcCatLt
        Char.UppercaseLetter      -> LcCatLu
        Char.NonSpacingMark       -> LcCatM
        Char.SpacingCombiningMark -> LcCatM
        Char.EnclosingMark        -> LcCatM
        Char.LetterNumber         -> LcCatNl
        Char.OtherNumber          -> LcCatNo
        Char.ConnectorPunctuation -> LcCatPc
        Char.DashPunctuation      -> LcCatPd
        Char.ClosePunctuation     -> LcCatPe
        Char.FinalQuote           -> LcCatPf
        Char.InitialQuote         -> LcCatPi
        Char.OtherPunctuation     -> LcCatPo
        Char.OpenPunctuation      -> LcCatPs
        Char.MathSymbol           -> LcCatS
        Char.CurrencySymbol       -> LcCatS
        Char.ModifierSymbol       -> LcCatS
        Char.OtherSymbol          -> LcCatS
        Char.LineSeparator        -> LcCatZl
        Char.ParagraphSeparator   -> LcCatZp
        Char.Space                -> LcCatZs
        _                         -> LcOther


catFormat :: EnumSet.EnumSet CodeUnit
catFormat = EnumSet.fromList
    [ CodeUnitByPoint LcU00200E
    , CodeUnitByPoint LcU00200F
    , CodeUnitOtherByCat LcCatCf
    ]

catLowercaseLetter :: EnumSet.EnumSet CodeUnit
catLowercaseLetter = EnumSet.fromList
    [ CodeUnitByPoint LcU000061
    , CodeUnitByPoint LcU000062
    , CodeUnitByPoint LcU000063
    , CodeUnitByPoint LcU000064
    , CodeUnitByPoint LcU000065
    , CodeUnitByPoint LcU000066
    , CodeUnitByPoint LcU000067
    , CodeUnitByPoint LcU000068
    , CodeUnitByPoint LcU000069
    , CodeUnitByPoint LcU00006A
    , CodeUnitByPoint LcU00006B
    , CodeUnitByPoint LcU00006C
    , CodeUnitByPoint LcU00006D
    , CodeUnitByPoint LcU00006E
    , CodeUnitByPoint LcU00006F
    , CodeUnitByPoint LcU000070
    , CodeUnitByPoint LcU000071
    , CodeUnitByPoint LcU000072
    , CodeUnitByPoint LcU000073
    , CodeUnitByPoint LcU000074
    , CodeUnitByPoint LcU000075
    , CodeUnitByPoint LcU000076
    , CodeUnitByPoint LcU000077
    , CodeUnitByPoint LcU000078
    , CodeUnitByPoint LcU000079
    , CodeUnitByPoint LcU00007A
    , CodeUnitByPoint LcU0003BB
    , CodeUnitOtherByCat LcCatLl
    ]

catModifierLetter :: EnumSet.EnumSet CodeUnit
catModifierLetter = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatLm
    ]

catOtherLetter :: EnumSet.EnumSet CodeUnit
catOtherLetter = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatLo
    ]

catTitlecaseLetter :: EnumSet.EnumSet CodeUnit
catTitlecaseLetter = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatLt
    ]

catUppercaseLetter :: EnumSet.EnumSet CodeUnit
catUppercaseLetter = EnumSet.fromList
    [ CodeUnitByPoint LcU000041
    , CodeUnitByPoint LcU000042
    , CodeUnitByPoint LcU000043
    , CodeUnitByPoint LcU000044
    , CodeUnitByPoint LcU000045
    , CodeUnitByPoint LcU000046
    , CodeUnitByPoint LcU000047
    , CodeUnitByPoint LcU000048
    , CodeUnitByPoint LcU000049
    , CodeUnitByPoint LcU00004A
    , CodeUnitByPoint LcU00004B
    , CodeUnitByPoint LcU00004C
    , CodeUnitByPoint LcU00004D
    , CodeUnitByPoint LcU00004E
    , CodeUnitByPoint LcU00004F
    , CodeUnitByPoint LcU000050
    , CodeUnitByPoint LcU000051
    , CodeUnitByPoint LcU000052
    , CodeUnitByPoint LcU000053
    , CodeUnitByPoint LcU000054
    , CodeUnitByPoint LcU000055
    , CodeUnitByPoint LcU000056
    , CodeUnitByPoint LcU000057
    , CodeUnitByPoint LcU000058
    , CodeUnitByPoint LcU000059
    , CodeUnitByPoint LcU00005A
    , CodeUnitOtherByCat LcCatLu
    ]

catMark :: EnumSet.EnumSet CodeUnit
catMark = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatM
    ]

catDecimalNumber :: EnumSet.EnumSet CodeUnit
catDecimalNumber = EnumSet.unions
    [ groupDigit0
    , groupDigit1
    , groupDigit2
    , groupDigit3
    , groupDigit4
    , groupDigit5
    , groupDigit6
    , groupDigit7
    , groupDigit8
    , groupDigit9
    ]

catLetterNumber :: EnumSet.EnumSet CodeUnit
catLetterNumber = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatNl
    ]

catOtherNumber :: EnumSet.EnumSet CodeUnit
catOtherNumber = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatNo
    ]

catConnectorPunctuation :: EnumSet.EnumSet CodeUnit
catConnectorPunctuation = EnumSet.fromList
    [ CodeUnitByPoint LcU00005F
    , CodeUnitOtherByCat LcCatPc
    ]

catDashPunctuation :: EnumSet.EnumSet CodeUnit
catDashPunctuation = EnumSet.fromList
    [ CodeUnitByPoint LcU00002D
    , CodeUnitOtherByCat LcCatPd
    ]

catClosePunctuation :: EnumSet.EnumSet CodeUnit
catClosePunctuation = EnumSet.fromList
    [ CodeUnitByPoint LcU000029
    , CodeUnitByPoint LcU00005D
    , CodeUnitByPoint LcU00007D
    , CodeUnitByPoint LcU002775
    , CodeUnitByPoint LcU002984
    , CodeUnitOtherByCat LcCatPe
    ]

catFinalPunctuation :: EnumSet.EnumSet CodeUnit
catFinalPunctuation = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatPf
    ]

catInitialPunctuation :: EnumSet.EnumSet CodeUnit
catInitialPunctuation = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatPi
    ]

catOtherPunctuation :: EnumSet.EnumSet CodeUnit
catOtherPunctuation = EnumSet.fromList
    [ CodeUnitByPoint LcU000022
    , CodeUnitByPoint LcU000023
    , CodeUnitByPoint LcU000027
    , CodeUnitByPoint LcU00002A
    , CodeUnitByPoint LcU00002C
    , CodeUnitByPoint LcU00002E
    , CodeUnitByPoint LcU00002F
    , CodeUnitByPoint LcU00003A
    , CodeUnitByPoint LcU00003B
    , CodeUnitByPoint LcU000040
    , CodeUnitByPoint LcU00005C
    , CodeUnitByPoint LcU002021
    , CodeUnitByPoint LcU002026
    , CodeUnitByPoint LcU00FE5F
    , CodeUnitOtherByCat LcCatPo
    ]

catOpenPunctuation :: EnumSet.EnumSet CodeUnit
catOpenPunctuation = EnumSet.fromList
    [ CodeUnitByPoint LcU000028
    , CodeUnitByPoint LcU00005B
    , CodeUnitByPoint LcU00007B
    , CodeUnitByPoint LcU002774
    , CodeUnitByPoint LcU002983
    , CodeUnitOtherByCat LcCatPs
    ]

catPunctuation :: EnumSet.EnumSet CodeUnit
catPunctuation = mconcat
    [ catClosePunctuation
    , catConnectorPunctuation
    , catDashPunctuation
    , catFinalPunctuation
    , catInitialPunctuation
    , catOpenPunctuation
    , catOtherPunctuation
    ]

catSymbol :: EnumSet.EnumSet CodeUnit
catSymbol = EnumSet.fromList
    [ CodeUnitByPoint LcU00002B
    , CodeUnitByPoint LcU00003C
    , CodeUnitByPoint LcU00003D
    , CodeUnitByPoint LcU00003E
    , CodeUnitByPoint LcU00005E
    , CodeUnitByPoint LcU00007E
    , CodeUnitByPoint LcU002200
    , CodeUnitByPoint LcU0029CF
    , CodeUnitByPoint LcU0029D0
    , CodeUnitOtherByCat LcCatS
    ]

catLineSeparator :: EnumSet.EnumSet CodeUnit
catLineSeparator = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatZl
    ]

catParagraphSeparator :: EnumSet.EnumSet CodeUnit
catParagraphSeparator = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatZp
    ]

catSpaceSeparator :: EnumSet.EnumSet CodeUnit
catSpaceSeparator = EnumSet.fromList
    [ CodeUnitByPoint LcU000020
    , CodeUnitOtherByCat LcCatZs
    ]

groupDigit0 :: EnumSet.EnumSet CodeUnit
groupDigit0 = EnumSet.fromList
    [ CodeUnitByPoint LcU000030
    , CodeUnitOtherByGroup LcGNdDigit0
    ]

groupDigit1 :: EnumSet.EnumSet CodeUnit
groupDigit1 = EnumSet.fromList
    [ CodeUnitOtherByGroup LcGNdDigit1
    ]

groupDigit2 :: EnumSet.EnumSet CodeUnit
groupDigit2 = EnumSet.fromList
    [ CodeUnitOtherByGroup LcGNdDigit2
    ]

groupDigit3 :: EnumSet.EnumSet CodeUnit
groupDigit3 = EnumSet.fromList
    [ CodeUnitOtherByGroup LcGNdDigit3
    ]

groupDigit4 :: EnumSet.EnumSet CodeUnit
groupDigit4 = EnumSet.fromList
    [ CodeUnitOtherByGroup LcGNdDigit4
    ]

groupDigit5 :: EnumSet.EnumSet CodeUnit
groupDigit5 = EnumSet.fromList
    [ CodeUnitOtherByGroup LcGNdDigit5
    ]

groupDigit6 :: EnumSet.EnumSet CodeUnit
groupDigit6 = EnumSet.fromList
    [ CodeUnitOtherByGroup LcGNdDigit6
    ]

groupDigit7 :: EnumSet.EnumSet CodeUnit
groupDigit7 = EnumSet.fromList
    [ CodeUnitOtherByGroup LcGNdDigit7
    ]

groupDigit8 :: EnumSet.EnumSet CodeUnit
groupDigit8 = EnumSet.fromList
    [ CodeUnitOtherByGroup LcGNdDigit8
    ]

groupDigit9 :: EnumSet.EnumSet CodeUnit
groupDigit9 = EnumSet.fromList
    [ CodeUnitOtherByGroup LcGNdDigit9
    ]

pattern LcAEndOfLine :: CodeUnit
pattern LcAEndOfLine = CodeUnitByPoint LcU00000A

pattern LcACarriageReturn :: CodeUnit
pattern LcACarriageReturn = CodeUnitByPoint LcU00000D

pattern LcAPlusSign :: CodeUnit
pattern LcAPlusSign = CodeUnitByPoint LcU00002B

pattern LcAHyphenMinus :: CodeUnit
pattern LcAHyphenMinus = CodeUnitByPoint LcU00002D

pattern LcAFullStop :: CodeUnit
pattern LcAFullStop = CodeUnitByPoint LcU00002E

pattern LcALowLine :: CodeUnit
pattern LcALowLine = CodeUnitByPoint LcU00005F
