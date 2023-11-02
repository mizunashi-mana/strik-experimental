module Language.Quell.Parsing.Lexer.CodeUnit where

import Language.Quell.Prelude

import qualified Data.EnumSet as EnumSet
import qualified Data.Char as Char


type T = CodeUnit

data CodeUnit
    = CodeUnitByPoint ByPoint
    | CodeUnitOtherByCat ByCat
    deriving (Eq, Show, Ord)

instance Enum CodeUnit where
    toEnum = \case
        i | i <= byPointMaxBound -> CodeUnitByPoint do toEnum i
        i -> CodeUnitOtherByCat do toEnum do i - byPointMaxBound
        where
            byPointMaxBound = fromEnum do maxBound :: ByPoint

    fromEnum = \case
        CodeUnitByPoint u -> fromEnum u
        CodeUnitOtherByCat u -> fromEnum u + fromEnum do maxBound :: ByPoint

instance Bounded CodeUnit where
    minBound = CodeUnitByPoint minBound
    maxBound = CodeUnitOtherByCat maxBound

data ByPoint
    -- https://www.compart.com/en/unicode/
    = LcU0009 -- '\t'
    | LcU000A -- '\n'
    | LcU000B -- '\v'
    | LcU000C -- '\f'
    | LcU000D -- '\r'
    | LcU0020 -- ' '
    | LcU0022 -- '"'
    | LcU0023 -- '#'
    | LcU0027 -- '\''
    | LcU0028 -- '('
    | LcU0029 -- ')'
    | LcU002A -- '*'
    | LcU002B -- '+'
    | LcU002C -- ','
    | LcU002D -- '-'
    | LcU002E -- '.'
    | LcU002F -- '/'
    | LcU0030 -- '0'
    | LcU0031 -- '1'
    | LcU0032 -- '2'
    | LcU0033 -- '3'
    | LcU0034 -- '4'
    | LcU0035 -- '5'
    | LcU0036 -- '6'
    | LcU0037 -- '7'
    | LcU0038 -- '8'
    | LcU0039 -- '9'
    | LcU003A -- ':'
    | LcU003B -- ';'
    | LcU003C -- '<'
    | LcU003D -- '='
    | LcU003E -- '>'
    | LcU0040 -- '@'
    | LcU0041 -- 'A'
    | LcU0042 -- 'B'
    | LcU0043 -- 'C'
    | LcU0044 -- 'D'
    | LcU0045 -- 'E'
    | LcU0046 -- 'F'
    | LcU0047 -- 'G'
    | LcU0048 -- 'H'
    | LcU0049 -- 'I'
    | LcU004A -- 'J'
    | LcU004B -- 'K'
    | LcU004C -- 'L'
    | LcU004D -- 'M'
    | LcU004E -- 'N'
    | LcU004F -- 'O'
    | LcU0050 -- 'P'
    | LcU0051 -- 'Q'
    | LcU0052 -- 'R'
    | LcU0053 -- 'S'
    | LcU0054 -- 'T'
    | LcU0055 -- 'U'
    | LcU0056 -- 'V'
    | LcU0057 -- 'W'
    | LcU0058 -- 'X'
    | LcU0059 -- 'Y'
    | LcU005A -- 'Z'
    | LcU005B -- '['
    | LcU005C -- '\\'
    | LcU005D -- ']'
    | LcU005E -- '^'
    | LcU005F -- '_'
    | LcU0061 -- 'a'
    | LcU0062 -- 'b'
    | LcU0063 -- 'c'
    | LcU0064 -- 'd'
    | LcU0065 -- 'e'
    | LcU0066 -- 'f'
    | LcU0067 -- 'g'
    | LcU0068 -- 'h'
    | LcU0069 -- 'i'
    | LcU006A -- 'j'
    | LcU006B -- 'k'
    | LcU006C -- 'l'
    | LcU006D -- 'm'
    | LcU006E -- 'n'
    | LcU006F -- 'o'
    | LcU0070 -- 'p'
    | LcU0071 -- 'q'
    | LcU0072 -- 'r'
    | LcU0073 -- 's'
    | LcU0074 -- 't'
    | LcU0075 -- 'u'
    | LcU0076 -- 'v'
    | LcU0077 -- 'w'
    | LcU0078 -- 'x'
    | LcU0079 -- 'y'
    | LcU007A -- 'z'
    | LcU007B -- '{'
    | LcU007D -- '}'
    | LcU007E -- '~'
    | LcU03BB -- 'λ'
    | LcU200E -- Left-to-Right Mark
    | LcU200F -- Right-to-Left Mark
    | LcU2026 -- '…'
    | LcU2021 -- '‡'
    | LcU2200 -- '∀'
    | LcU2774 -- '❴'
    | LcU2775 -- '❵'
    | LcU2983 -- '⦃'
    | LcU2984 -- '⦄'
    | LcU29CF -- '⧏'
    | LcU29D0 -- '⧐'
    | LcUFE5F -- '﹟'
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
    0x0009 -> CodeUnitByPoint LcU0009
    0x000A -> CodeUnitByPoint LcU000A
    0x000B -> CodeUnitByPoint LcU000B
    0x000C -> CodeUnitByPoint LcU000C
    0x000D -> CodeUnitByPoint LcU000D
    0x0020 -> CodeUnitByPoint LcU0020
    0x0022 -> CodeUnitByPoint LcU0022
    0x0023 -> CodeUnitByPoint LcU0023
    0x0027 -> CodeUnitByPoint LcU0027
    0x0028 -> CodeUnitByPoint LcU0028
    0x0029 -> CodeUnitByPoint LcU0029
    0x002A -> CodeUnitByPoint LcU002A
    0x002B -> CodeUnitByPoint LcU002B
    0x002C -> CodeUnitByPoint LcU002C
    0x002D -> CodeUnitByPoint LcU002D
    0x002E -> CodeUnitByPoint LcU002E
    0x002F -> CodeUnitByPoint LcU002F
    0x0030 -> CodeUnitByPoint LcU0030
    0x0031 -> CodeUnitByPoint LcU0031
    0x0032 -> CodeUnitByPoint LcU0032
    0x0033 -> CodeUnitByPoint LcU0033
    0x0034 -> CodeUnitByPoint LcU0034
    0x0035 -> CodeUnitByPoint LcU0035
    0x0036 -> CodeUnitByPoint LcU0036
    0x0037 -> CodeUnitByPoint LcU0037
    0x0038 -> CodeUnitByPoint LcU0038
    0x0039 -> CodeUnitByPoint LcU0039
    0x003A -> CodeUnitByPoint LcU003A
    0x003B -> CodeUnitByPoint LcU003B
    0x003C -> CodeUnitByPoint LcU003C
    0x003D -> CodeUnitByPoint LcU003D
    0x003E -> CodeUnitByPoint LcU003E
    0x0040 -> CodeUnitByPoint LcU0040
    0x0041 -> CodeUnitByPoint LcU0041
    0x0042 -> CodeUnitByPoint LcU0042
    0x0043 -> CodeUnitByPoint LcU0043
    0x0044 -> CodeUnitByPoint LcU0044
    0x0045 -> CodeUnitByPoint LcU0045
    0x0046 -> CodeUnitByPoint LcU0046
    0x0047 -> CodeUnitByPoint LcU0047
    0x0048 -> CodeUnitByPoint LcU0048
    0x0049 -> CodeUnitByPoint LcU0049
    0x004A -> CodeUnitByPoint LcU004A
    0x004B -> CodeUnitByPoint LcU004B
    0x004C -> CodeUnitByPoint LcU004C
    0x004D -> CodeUnitByPoint LcU004D
    0x004E -> CodeUnitByPoint LcU004E
    0x004F -> CodeUnitByPoint LcU004F
    0x0050 -> CodeUnitByPoint LcU0050
    0x0051 -> CodeUnitByPoint LcU0051
    0x0052 -> CodeUnitByPoint LcU0052
    0x0053 -> CodeUnitByPoint LcU0053
    0x0054 -> CodeUnitByPoint LcU0054
    0x0055 -> CodeUnitByPoint LcU0055
    0x0056 -> CodeUnitByPoint LcU0056
    0x0057 -> CodeUnitByPoint LcU0057
    0x0058 -> CodeUnitByPoint LcU0058
    0x0059 -> CodeUnitByPoint LcU0059
    0x005A -> CodeUnitByPoint LcU005A
    0x005B -> CodeUnitByPoint LcU005B
    0x005C -> CodeUnitByPoint LcU005C
    0x005D -> CodeUnitByPoint LcU005D
    0x005E -> CodeUnitByPoint LcU005E
    0x005F -> CodeUnitByPoint LcU005F
    0x0061 -> CodeUnitByPoint LcU0061
    0x0062 -> CodeUnitByPoint LcU0062
    0x0063 -> CodeUnitByPoint LcU0063
    0x0064 -> CodeUnitByPoint LcU0064
    0x0065 -> CodeUnitByPoint LcU0065
    0x0066 -> CodeUnitByPoint LcU0066
    0x0067 -> CodeUnitByPoint LcU0067
    0x0068 -> CodeUnitByPoint LcU0068
    0x0069 -> CodeUnitByPoint LcU0069
    0x006A -> CodeUnitByPoint LcU006A
    0x006B -> CodeUnitByPoint LcU006B
    0x006C -> CodeUnitByPoint LcU006C
    0x006D -> CodeUnitByPoint LcU006D
    0x006E -> CodeUnitByPoint LcU006E
    0x006F -> CodeUnitByPoint LcU006F
    0x0070 -> CodeUnitByPoint LcU0070
    0x0071 -> CodeUnitByPoint LcU0071
    0x0072 -> CodeUnitByPoint LcU0072
    0x0073 -> CodeUnitByPoint LcU0073
    0x0074 -> CodeUnitByPoint LcU0074
    0x0075 -> CodeUnitByPoint LcU0075
    0x0076 -> CodeUnitByPoint LcU0076
    0x0077 -> CodeUnitByPoint LcU0077
    0x0078 -> CodeUnitByPoint LcU0078
    0x0079 -> CodeUnitByPoint LcU0079
    0x007A -> CodeUnitByPoint LcU007A
    0x007B -> CodeUnitByPoint LcU007B
    0x007D -> CodeUnitByPoint LcU007D
    0x007E -> CodeUnitByPoint LcU007E
    0x03BB -> CodeUnitByPoint LcU03BB
    0x200E -> CodeUnitByPoint LcU200E
    0x200F -> CodeUnitByPoint LcU200F
    0x2021 -> CodeUnitByPoint LcU2021
    0x2026 -> CodeUnitByPoint LcU2026
    0x2200 -> CodeUnitByPoint LcU2200
    0x2774 -> CodeUnitByPoint LcU2774
    0x2775 -> CodeUnitByPoint LcU2775
    0x2983 -> CodeUnitByPoint LcU2983
    0x2984 -> CodeUnitByPoint LcU2984
    0x29CF -> CodeUnitByPoint LcU29CF
    0x29D0 -> CodeUnitByPoint LcU29D0
    0xFE5F -> CodeUnitByPoint LcUFE5F
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
        Char.DecimalNumber        -> LcCatNd
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
    [ CodeUnitByPoint LcU200E
    , CodeUnitByPoint LcU200F
    , CodeUnitOtherByCat LcCatCf
    ]

catLowercaseLetter :: EnumSet.EnumSet CodeUnit
catLowercaseLetter = EnumSet.fromList
    [ CodeUnitByPoint LcU0061
    , CodeUnitByPoint LcU0062
    , CodeUnitByPoint LcU0063
    , CodeUnitByPoint LcU0064
    , CodeUnitByPoint LcU0065
    , CodeUnitByPoint LcU0066
    , CodeUnitByPoint LcU0067
    , CodeUnitByPoint LcU0068
    , CodeUnitByPoint LcU0069
    , CodeUnitByPoint LcU006A
    , CodeUnitByPoint LcU006B
    , CodeUnitByPoint LcU006C
    , CodeUnitByPoint LcU006D
    , CodeUnitByPoint LcU006E
    , CodeUnitByPoint LcU006F
    , CodeUnitByPoint LcU0070
    , CodeUnitByPoint LcU0071
    , CodeUnitByPoint LcU0072
    , CodeUnitByPoint LcU0073
    , CodeUnitByPoint LcU0074
    , CodeUnitByPoint LcU0075
    , CodeUnitByPoint LcU0076
    , CodeUnitByPoint LcU0077
    , CodeUnitByPoint LcU0078
    , CodeUnitByPoint LcU0079
    , CodeUnitByPoint LcU007A
    , CodeUnitByPoint LcU03BB
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
    [ CodeUnitByPoint LcU0041
    , CodeUnitByPoint LcU0042
    , CodeUnitByPoint LcU0043
    , CodeUnitByPoint LcU0044
    , CodeUnitByPoint LcU0045
    , CodeUnitByPoint LcU0046
    , CodeUnitByPoint LcU0047
    , CodeUnitByPoint LcU0048
    , CodeUnitByPoint LcU0049
    , CodeUnitByPoint LcU004A
    , CodeUnitByPoint LcU004B
    , CodeUnitByPoint LcU004C
    , CodeUnitByPoint LcU004D
    , CodeUnitByPoint LcU004E
    , CodeUnitByPoint LcU004F
    , CodeUnitByPoint LcU0050
    , CodeUnitByPoint LcU0051
    , CodeUnitByPoint LcU0052
    , CodeUnitByPoint LcU0053
    , CodeUnitByPoint LcU0054
    , CodeUnitByPoint LcU0055
    , CodeUnitByPoint LcU0056
    , CodeUnitByPoint LcU0057
    , CodeUnitByPoint LcU0058
    , CodeUnitByPoint LcU0059
    , CodeUnitByPoint LcU005A
    , CodeUnitOtherByCat LcCatLu
    ]

catMark :: EnumSet.EnumSet CodeUnit
catMark = EnumSet.fromList
    [ CodeUnitOtherByCat LcCatM
    ]

catDecimalNumber :: EnumSet.EnumSet CodeUnit
catDecimalNumber = EnumSet.fromList
    [ CodeUnitByPoint LcU0030
    , CodeUnitByPoint LcU0031
    , CodeUnitByPoint LcU0032
    , CodeUnitByPoint LcU0033
    , CodeUnitByPoint LcU0034
    , CodeUnitByPoint LcU0035
    , CodeUnitByPoint LcU0036
    , CodeUnitByPoint LcU0037
    , CodeUnitByPoint LcU0038
    , CodeUnitByPoint LcU0039
    , CodeUnitOtherByCat LcCatNd
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
    [ CodeUnitByPoint LcU005F
    , CodeUnitOtherByCat LcCatPc
    ]

catDashPunctuation :: EnumSet.EnumSet CodeUnit
catDashPunctuation = EnumSet.fromList
    [ CodeUnitByPoint LcU002D
    , CodeUnitOtherByCat LcCatPd
    ]

catClosePunctuation :: EnumSet.EnumSet CodeUnit
catClosePunctuation = EnumSet.fromList
    [ CodeUnitByPoint LcU0029
    , CodeUnitByPoint LcU005D
    , CodeUnitByPoint LcU007D
    , CodeUnitByPoint LcU2775
    , CodeUnitByPoint LcU2984
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
    [ CodeUnitByPoint LcU0022
    , CodeUnitByPoint LcU0023
    , CodeUnitByPoint LcU0027
    , CodeUnitByPoint LcU002A
    , CodeUnitByPoint LcU002C
    , CodeUnitByPoint LcU002E
    , CodeUnitByPoint LcU002F
    , CodeUnitByPoint LcU003A
    , CodeUnitByPoint LcU003B
    , CodeUnitByPoint LcU0040
    , CodeUnitByPoint LcU005C
    , CodeUnitByPoint LcU2021
    , CodeUnitByPoint LcU2026
    , CodeUnitByPoint LcUFE5F
    , CodeUnitOtherByCat LcCatPo
    ]

catOpenPunctuation :: EnumSet.EnumSet CodeUnit
catOpenPunctuation = EnumSet.fromList
    [ CodeUnitByPoint LcU0028
    , CodeUnitByPoint LcU005B
    , CodeUnitByPoint LcU007B
    , CodeUnitByPoint LcU2774
    , CodeUnitByPoint LcU2983
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
    [ CodeUnitByPoint LcU002B
    , CodeUnitByPoint LcU003C
    , CodeUnitByPoint LcU003D
    , CodeUnitByPoint LcU003E
    , CodeUnitByPoint LcU005E
    , CodeUnitByPoint LcU007E
    , CodeUnitByPoint LcU2200
    , CodeUnitByPoint LcU29CF
    , CodeUnitByPoint LcU29D0
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
    [ CodeUnitByPoint LcU0020
    , CodeUnitOtherByCat LcCatZs
    ]

pattern LcGEndOfLine :: CodeUnit
pattern LcGEndOfLine = CodeUnitByPoint LcU000A

pattern LcGCarriageReturn :: CodeUnit
pattern LcGCarriageReturn = CodeUnitByPoint LcU000D

pattern LcGPlusSign :: CodeUnit
pattern LcGPlusSign = CodeUnitByPoint LcU002B

pattern LcGHyphenMinus :: CodeUnit
pattern LcGHyphenMinus = CodeUnitByPoint LcU002D

pattern LcGFullStop :: CodeUnit
pattern LcGFullStop = CodeUnitByPoint LcU002E
