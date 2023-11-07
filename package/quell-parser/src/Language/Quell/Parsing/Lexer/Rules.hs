{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Quell.Parsing.Lexer.Rules where

import           Language.Quell.Prelude

import qualified Data.EnumSet                          as EnumSet
import qualified Language.Haskell.TH                   as TH
import qualified Language.Lexer.Tlex                   as Tlex
import qualified Language.Lexer.Tlex.Plugin.TH         as TlexTH
import qualified Language.Quell.Data.TextId            as TextId
import qualified Language.Quell.Frontend.Data.Token    as Token
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit


buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    stateTy <- [t|LexerState|]
    codeUnitTy <- [t|LexerCodeUnit|]
    actionTy <- [t|LexerAction|]
    let lexer = TlexTH.buildTHScanner codeUnitTy stateTy actionTy lexerRules
    TlexTH.outputScanner lexer


data LexerAction
    = WithToken Token.T
    | WithIdType IdType
    | WithKwToken
    | WithWhiteSpace
    | LexIdFreeId
    | LexLitOrLitPartString
    | LexLitRational
    | LexLitInteger
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
lexerRules = do
    literalPartRules
    literalRules
    specialCharRules
    keywordIdRules
    keywordSymRules
    identifierRules


identifierRules :: ScannerBuilder ()
identifierRules = do
    initialRule varIdP [||withIdType Token.IdVarId||]
    initialRule conIdP [||withIdType Token.IdConId||]
    initialRule varSymP [||withIdType Token.IdVarSym||]
    initialRule conSymP [||withIdType Token.IdConSym||]
    initialRule freeIdP [||LexIdFreeId||]

varIdP = idSmallCharP <> Tlex.manyP idCharP
conIdP = idLargeCharP <> Tlex.manyP idCharP
varSymP = symNormalCharP <> Tlex.manyP symCharP
conSymP = symSpCharP <> Tlex.manyP symCharP
freeIdP = keywordPrefixCharP <> stringP


keywordPrefixedRules :: ScannerBuilder ()
keywordPrefixedRules = do
    initialRule (keywordPrefixCharP <> Tlex.someP idCharP) [||WithKwToken||]
    initialRule (keywordPrefixCharP <> Tlex.someP symCharP) [||WithKwToken||]
    initialRule (keywordPrefixCharP <> charsP ['{']) [||withLexToken Token.KwBraceOpen||]
    initialRule (keywordPrefixCharP <> charsP ['[']) [||withLexToken Token.KwBrackOpen||]
    initialRule (keywordPrefixCharP <> charsP ['(']) [||withLexToken Token.KwParenOpen||]

-- | All identifiers parsed as varIds.
keywordIdRules :: ScannerBuilder ()
keywordIdRules = pure ()

-- | All symbols parsed as varSyms.
keywordSymRules :: ScannerBuilder ()
keywordSymRules = pure ()


literalPartRules :: ScannerBuilder ()
literalPartRules = do
    interpStringPartRules

literalRules :: ScannerBuilder ()
literalRules = do
    stringRules
    rationalRules
    integerRules

rationalRules :: ScannerBuilder ()
rationalRules = do
    initialRule (Tlex.maybeP signCharP <> nonZeroDecimalP <> numDotSymCharP <> decimalP) [||LexLitRational||]

integerRules :: ScannerBuilder ()
integerRules = do
    initialRule (numberPrefixP <> charsP ['x', 'X'] <> heximalP) [||LexLitInteger||]
    initialRule (numberPrefixP <> charsP ['d', 'D'] <> decimalP) [||LexLitInteger||]
    initialRule (Tlex.maybeP signCharP <> nonZeroDecimalP) [||LexLitInteger||]

numberPrefixP = Tlex.maybeP signCharP <> zeroCharP

nonZeroDecimalP = charSetP nonZeroDigitCharCs <> Tlex.manyP digitOrUscoreCharP
nonZeroDigitCharCs = digitCharCs `EnumSet.difference` zeroCharCs

decimalP = digitCharP <> Tlex.manyP digitOrUscoreCharP
heximalP = hexitCharP <> Tlex.manyP hexitOrUscoreCharP

signCharP = charSetP signCharCs
signCharCs = charsCs ['+', '-']

zeroCharP = charSetP zeroCharCs
zeroCharCs = unitsCs [CodeUnit.CodeUnitOtherByGroup CodeUnit.LcGNdDigit0]

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

numSepSymCharP = charSetP numSepSymCharCs
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


interpStringPartRules :: ScannerBuilder ()
interpStringPartRules = do
    initialRule interpStringStartP [||LexLitOrLitPartString||]
    initialRule interpStringContP [||LexLitOrLitPartString||]
    initialRule interpStringEndP [||LexLitOrLitPartString||]

stringRules :: ScannerBuilder ()
stringRules = do
    initialRule stringP [||LexLitOrLitPartString||]

stringP = strSepCharP <> Tlex.manyP interpStringGraphicP <> strSepCharP

interpStringStartP = strSepCharP <> Tlex.manyP interpStringGraphicP <> interpOpenP
interpStringContP = interpCloseP <> Tlex.manyP interpStringGraphicP <> interpOpenP
interpStringEndP = interpCloseP <> Tlex.manyP interpStringGraphicP <> strSepCharP

interpOpenP = interpOpenCharP <> charsP ['{']
interpCloseP = keywordPrefixCharP <> charsP ['}']

escapeOpenCharP = charSetP escapeOpenCharCs
escapeOpenCharCs = EnumSet.unions
    [
        charsCs [
            '\\'
        ]
    ]

interpOpenCharP = charSetP interpOpenCharCs
interpOpenCharCs = keywordPrefixCharCs

strSepCharP = charSetP strSepCharCs
strSepCharCs = EnumSet.unions
    [
        charsCs [
            '"'
        ]
    ]

interpStringGraphicP = Tlex.orP
    [
        uniEscapeP,
        bstrGraphicP
    ]

bstrGraphicP = Tlex.orP
    [
        byteEscapeP,
        charEscapeP,
        bstrGraphicCharP
    ]

bstrGraphicCharP = charSetP bstrGraphicCharCs
bstrGraphicCharCs = EnumSet.unions
    [
        whiteCharCs,
        bstrOtherGraphicCharCs
    ]

bstrOtherGraphicCharCs = graphicCharCs `EnumSet.difference` EnumSet.unions
    [
        interpStringSepCharCs,
        escapeOpenCharCs,
        interpOpenCharCs
    ]

uniEscapeP = uniEscapePrefixP <> Tlex.someP hexitCharP <> strP "}"
uniEscapePrefixP = escapeOpenCharP <> charsP ['u', 'U'] <> strP "{"

byteEscapeP = byteEscapePrefixP <> hexitCharP <> hexitCharP
byteEscapePrefixP = escapeOpenCharP <> charsP ['x', 'X']

charEscapeP = escapeOpenCharP <> charSetP charescCharCs

charescCharCs = EnumSet.unions
    [
        charsCs [
            '0', 'a', 'b', 'f', 'n', 'r', 't', 'v'
        ],
        escapeOpenCharCs,
        strSepCharCs,
        interpOpenCharCs
    ]


whiteSpaceRules :: ScannerBuilder ()
whiteSpaceRules = do
    initialRule (Tlex.someP whiteCharP) [||WithWhiteSpace||]

    commentRules


commentRules :: ScannerBuilder ()
commentRules = do
    initialRule lineCommentOpenP [||LexCommentLineWithContent||]
    initialRule multilineCommentOpenP [||LexCommentMultilineWithContent||]

lineCommentOpenP = strP "//"

multilineCommentOpenP = strP "/*"
multilineCommentCloseP = strP "*/"

any1lCharP = charSetP any1lCharCs
any1lCharCs = EnumSet.unions
    [
        graphicCharCs,
        spaceCharCs
    ]

anyCharP = charSetP anyCharCs
anyCharCs = EnumSet.unions
    [
        graphicCharCs,
        whiteCharCs
    ]


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

idCharP = charSetP idCharCs
idCharCs = EnumSet.unions
    [
        idSmallCharCs,
        idLargeCharCs,
        digitCharCs,
        otherCharCs
    ]

idSmallCharP = charSetP idSmallCharCs
idSmallCharCs = smallCharCs

idLargeCharP = charSetP idLargeCharCs
idLargeCharCs = largeCharCs

symCharP = charSetP symCharCs
symCharCs = EnumSet.unions
    [
        symNormalCharCs,
        symSpCharCs,
        otherCharCs
    ]

symNormalCharP = charSetP symNormalCharCs
symNormalCharCs = symbolCharCs `EnumSet.difference` symSpCharCs

symSpCharP = charSetP symSpCharCs
symSpCharCs = EnumSet.unions
    [
        charsCs [
            '~'
        ]
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

spaceCharCs = EnumSet.unions
    [
        charsCs [
            '\t',
            '\x200E',
            '\x200F'
        ],
        CodeUnit.catSpaceSeparator
    ]

newlineP = Tlex.orP
    [
        strP "\r\n",
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

digit0P = charSetP CodeUnit.groupDigit0
digit1P = charSetP CodeUnit.groupDigit1
digit2P = charSetP CodeUnit.groupDigit2
digit3P = charSetP CodeUnit.groupDigit3
digit4P = charSetP CodeUnit.groupDigit4
digit5P = charSetP CodeUnit.groupDigit5
digit6P = charSetP CodeUnit.groupDigit6
digit7P = charSetP CodeUnit.groupDigit7
digit8P = charSetP CodeUnit.groupDigit8
digit9P = charSetP CodeUnit.groupDigit9
hexitAP = charsP ['a', 'A']
hexitBP = charsP ['b', 'B']
hexitCP = charsP ['c', 'C']
hexitDP = charsP ['d', 'D']
hexitEP = charsP ['e', 'E']
hexitFP = charsP ['f', 'F']

otherCharCs = EnumSet.unions
    [
        otherCatCharCs
    ] `EnumSet.difference` EnumSet.unions
    [
        whiteCharCs
    ]

otherCatCharCs = EnumSet.unions
    [
        CodeUnit.catModifierLetter,
        CodeUnit.catMark,
        CodeUnit.catLetterNumber,
        CodeUnit.catOtherNumber,
        CodeUnit.catFormat
    ]

specialCharRules :: ScannerBuilder ()
specialCharRules = do
    initialRule (strP "{") [||withLexToken Token.SpBraceOpen||]
    initialRule (strP "}") [||withLexToken Token.SpBraceClose||]
    initialRule (strP "[") [||withLexToken Token.SpBrackOpen||]
    initialRule (strP "]") [||withLexToken Token.SpBrackClose||]
    initialRule (strP "(") [||withLexToken Token.SpParenOpen||]
    initialRule (strP ")") [||withLexToken Token.SpParenClose||]
    initialRule (strP ";") [||withLexToken Token.SpSemi||]
    initialRule (strP ".") [||withLexToken Token.SpDot||]

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

otherSpecialCharCs = EnumSet.unions
    [
        keywordPrefixCharCs,
        interpStringSepCharCs,
        charsCs [
            '\''
        ]
    ]

keywordPrefixCharP = charSetP keywordPrefixCharCs
keywordPrefixCharCs = charsCs ['#']

interpStringSepCharCs = charsCs ['"']

otherGraphicCharCs = EnumSet.unions
    [
        otherGraphicCatCharCs
    ] `EnumSet.difference` EnumSet.unions
    [
        symbolCatCharCs,
        specialCharCs,
        otherSpecialCharCs
    ]

otherGraphicCatCharCs = EnumSet.unions
    [
        CodeUnit.catPunctuation
    ]


charSetP :: CharSet -> Pattern
charSetP = Tlex.straightEnumSetP

unitsP :: [CodeUnit.T] -> Pattern
unitsP us = charSetP do unitsCs us

charsP :: [Char] -> Pattern
charsP cs = charSetP do charsCs cs

chP :: Char -> Pattern
chP c = charSetP do charsCs [c]

strP :: StringLit -> Pattern
strP = foldMap chP


unitsCs :: [CodeUnit.T] -> CharSet
unitsCs = EnumSet.fromList

charsCs :: [Char] -> CharSet
charsCs cs = unitsCs do
    c <- cs
    pure case CodeUnit.fromChar c of
        x@CodeUnit.CodeUnitByPoint{}    -> x
        CodeUnit.CodeUnitOtherByGroup{} -> unsupportedChar c
        CodeUnit.CodeUnitOtherByCat{}   -> unsupportedChar c
    where
        unsupportedChar c = error do "Unsupported char: " <> show c
