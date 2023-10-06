{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Quell.Lexer.Rules where

import Language.Quell.Prelude

import qualified Language.Quell.Data.TextId as TextId
import qualified Language.Quell.Frontend.Data.Token as Token
import qualified Language.Quell.Lexer.CodeUnit as CodeUnit
import qualified Language.Lexer.Tlex.Plugin.TH as TlexTH
import qualified Language.Lexer.Tlex as Tlex
import qualified Data.EnumSet as EnumSet
import qualified Language.Haskell.TH as TH


data LexerAction
    = WithToken Token.T
    | WithIdType IdType
    | WithWhiteSpace
    | LexLitRationalWithDot
    | LexLitHeximalInteger
    | LexLitDecimalInteger
    | LexLitDefaultInteger
    | LexLitString
    | LexInterpStringStart
    | LexInterpStringContinue
    | LexCommentLineWithContent
    | LexCommentMultilineWithContent
    deriving (Eq, Show)

withLexToken :: Token.LexToken -> LexerAction
withLexToken t = WithToken do Token.TokLexeme t

withIdType :: (TextId.T -> Token.LexToken) -> LexerAction
withIdType t = WithIdType do IdType t


newtype IdType = IdType (TextId.T -> Token.LexToken)

instance Eq IdType where
    IdType t1 == IdType t2 = do
        let dummyId = TextId.UnsafeTextId do text "dummy"
        let lt1 = t1 dummyId
        let lt2 = t2 dummyId
        lt1 == lt2

instance Show IdType where
    show = \case
        IdType t -> show do t do TextId.UnsafeTextId do text "..."


data LexerState = Initial
    deriving (Eq, Ord, Show, Enum)


type LexerCodeUnit = CodeUnit.T
type CharSet = EnumSet.EnumSet LexerCodeUnit

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit


initialRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
initialRule = TlexTH.thLexRule [Initial]


lexerRules :: ScannerBuilder ()
lexerRules = undefined


specialRules :: ScannerBuilder ()
specialRules = do
    initialRule (stringP "{") [||withLexToken Token.SpBraceOpen||]
    initialRule (stringP "}") [||withLexToken Token.SpBraceClose||]
    initialRule (stringP "[") [||withLexToken Token.SpBrackOpen||]
    initialRule (stringP "]") [||withLexToken Token.SpBrackClose||]
    initialRule (stringP "(") [||withLexToken Token.SpParenOpen||]
    initialRule (stringP ")") [||withLexToken Token.SpParenClose||]
    initialRule (stringP ";") [||withLexToken Token.SpSemi||]
    initialRule (stringP ".") [||withLexToken Token.SpDot||]

specialCharCs :: CharSet
specialCharCs = EnumSet.unions
    [
        charsCs [
            '{',
            '}',
            '[',
            ']',
            '(',
            ')',
            ';',
            '.'
        ]
    ]


rationalRules :: ScannerBuilder ()
rationalRules = do
    initialRule (Tlex.maybeP signCharP <> nonZeroDecimalP <> numDotSymCharP <> decimalP) [||LexLitRationalWithDot||]

integerRules :: ScannerBuilder ()
integerRules = do
    initialRule (numberPrefixP <> charsP ['x', 'X'] <> heximalP) [||LexLitHeximalInteger||]
    initialRule (numberPrefixP <> charsP ['d', 'D'] <> decimalP) [||LexLitDecimalInteger||]
    initialRule (Tlex.maybeP signCharP <> nonZeroDecimalP) [||LexLitDefaultInteger||]

numberPrefixP = Tlex.maybeP signCharP <> zeroCharP

nonZeroDecimalP = charSetP nonZeroDigitCharCs <> Tlex.manyP digitOrUscoreCharP
nonZeroDigitCharCs = digitCharCs `EnumSet.difference` zeroCharCs

decimalP = digitCharP <> Tlex.manyP digitOrUscoreCharP
heximalP = hexitCharP <> Tlex.manyP hexitOrUscoreCharP

signCharP = charSetP signCharCs
signCharCs = charsCs ['+', '-']

zeroCharP = charSetP zeroCharCs
zeroCharCs = charsCs ['0']

digitOrUscoreCharP = charSetP digitOrUscoreCharCs
digitOrUscoreCharCs = EnumSet.unions
    [
        digitCharCs,
        numSepSymCharCs
    ]

hexitOrUscoreCharP = charSetP hexitOrUscoreCharCs
hexitOrUscoreCharCs = EnumSet.unions
    [
        hexitCharCs,
        numSepSymCharCs
    ]

numDotSymCharP = charSetP numDotSymCharCs
numDotSymCharCs = EnumSet.unions
    [
        charsCs [
            '.'
        ]
    ]

numSepSymCharCs = EnumSet.unions
    [
        charsCs [
            '_'
        ]
    ]

hexitCharP = charSetP hexitCharCs
hexitCharCs = EnumSet.unions
    [
        digitCharCs,
        charsCs [
            'A', 'B', 'C', 'D', 'E', 'F',
            'a', 'b', 'c', 'd', 'e', 'f'
        ]
    ]


escapeOpenCharCs = EnumSet.unions
    [
        charsCs [
            '\\'
        ]
    ]

interpOpenCharCs = keywordPrefixCharCs

strSepCharCs = EnumSet.unions
    [
        charsCs [
            '"'
        ]
    ]

interpStringGraphicP = EnumSet.unions
    [

    ]

charescCharCs = EnumSet.unions
    [
        charsCs [
            '0', 'a', 'b', 'f', 'n', 'r', 't', 'v'
        ],
        escapeOpenCharCs,
        strSepCharCs,
        interpOpenCharCs
    ]

byteescP = stringP "x" <> hexitCharP <> hexitCharP


whiteSpaceRules :: ScannerBuilder ()
whiteSpaceRules = do
    initialRule (Tlex.someP whiteCharP) [||WithWhiteSpace||]

    commentRules


commentRules :: ScannerBuilder ()
commentRules = do
    initialRule lineCommentOpenP [||LexCommentLineWithContent||]
    initialRule multilineCommentOpenP [||LexCommentMultilineWithContent||]

lineCommentOpenP :: Pattern
lineCommentOpenP = stringP "//"

multilineCommentOpenP :: Pattern
multilineCommentOpenP = commentOpenP

commentOpenP :: Pattern
commentOpenP = stringP "/*"

commentCloseP :: Pattern
commentCloseP = stringP "*/"

any1lCharCs :: CharSet
any1lCharCs = EnumSet.unions
    [
        graphicCharCs,
        spaceCharCs
    ]

anyCharCs :: CharSet
anyCharCs = EnumSet.unions
    [
        graphicCharCs,
        whiteCharCs
    ]


graphicCharCs :: CharSet
graphicCharCs = EnumSet.unions
    [
        smallCharCs,
        largeCharCs,
        symbolCharCs,
        digitCharCs,
        otherCharCs,
        specialCharCs,
        otherSpecialCharCs,
        otherGraphicCharCs
    ]

idCharCs :: CharSet
idCharCs = EnumSet.unions
    [
        smallCharCs,
        largeCharCs,
        digitCharCs,
        otherCharCs
    ]

symCharCs :: CharSet
symCharCs = EnumSet.unions
    [
        symbolCharCs,
        otherCharCs
    ]

whiteCharP = charSetP whiteCharCs
whiteCharCs = EnumSet.unions
    [
        charsCs [
            '\v'
        ],
        spaceCharCs,
        newlineCharCs
    ]

spaceCharCs :: CharSet
spaceCharCs = EnumSet.unions
    [
        charsCs [
            '\t',
            '\x200E',
            '\x200F'
        ],
        CodeUnit.catSpaceSeparator
    ]

newlineP :: Pattern
newlineP = Tlex.orP
    [
        stringP "\r\n",
        charSetP newlineCharCs
    ]

newlineCharCs :: CharSet
newlineCharCs = EnumSet.unions
    [
        charsCs [
            '\r',
            '\n',
            '\f'
        ],
        CodeUnit.catLineSeparator,
        CodeUnit.catParagraphSeparator
    ]

smallCharCs = EnumSet.unions
    [
        CodeUnit.catLowercaseLetter,
        CodeUnit.catOtherLetter,
        charsCs [
            '_'
        ]
    ]

largeCharCs = EnumSet.unions
    [
        CodeUnit.catUppercaseLetter,
        CodeUnit.catTitlecaseLetter
    ]

symbolCharCs = EnumSet.unions
    [
        symbolCatCharCs
    ] `EnumSet.difference` EnumSet.unions [
        specialCharCs
    ]

symbolCatCharCs = EnumSet.unions
    [
        CodeUnit.catConnectorPunctuation,
        CodeUnit.catDashPunctuation,
        CodeUnit.catOtherPunctuation,
        CodeUnit.catSymbol
    ]

digitCharP = charSetP digitCharCs
digitCharCs = EnumSet.unions
    [
        CodeUnit.catDecimalNumber
    ]

otherCharCs :: CharSet
otherCharCs = EnumSet.unions
    [
        otherCatCharCs
    ] `EnumSet.difference` EnumSet.unions
    [
        whiteCharCs
    ]

otherCatCharCs :: CharSet
otherCatCharCs = EnumSet.unions
    [
        CodeUnit.catModifierLetter,
        CodeUnit.catMark,
        CodeUnit.catLetterNumber,
        CodeUnit.catOtherNumber,
        CodeUnit.catFormat
    ]

otherSpecialCharCs :: CharSet
otherSpecialCharCs = EnumSet.unions
    [
        keywordPrefixCharCs,
        interpStringSepCharCs,
        charsCs [
            '\''
        ]
    ]

keywordPrefixCharCs :: CharSet
keywordPrefixCharCs = charsCs ['#']

interpStringSepCharCs :: CharSet
interpStringSepCharCs = charsCs ['"']

otherGraphicCharCs :: CharSet
otherGraphicCharCs = EnumSet.unions
    [
        otherGraphicCatCharCs
    ] `EnumSet.difference` EnumSet.unions
    [
        symbolCatCharCs,
        specialCharCs,
        otherSpecialCharCs
    ]

otherGraphicCatCharCs :: CharSet
otherGraphicCatCharCs = EnumSet.unions
    [
        CodeUnit.catPunctuation
    ]


charSetP :: CharSet -> Pattern
charSetP = Tlex.straightEnumSetP

charsP :: [Char] -> Pattern
charsP cs = charSetP do charsCs cs

chP :: Char -> Pattern
chP c = charSetP do charsCs [c]

stringP :: StringLit -> Pattern
stringP = foldMap chP


charsCs :: [Char] -> CharSet
charsCs cs = EnumSet.fromList do
    c <- cs
    pure case CodeUnit.fromChar c of
        x@CodeUnit.CodeUnitByPoint{} -> x
        CodeUnit.CodeUnitOtherByCat{} -> error do "Unsupported char: " <> show c
