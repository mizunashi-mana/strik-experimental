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
    | LexLitRationalWithoutDot
    | LexLitHeximalInteger
    | LexLitDecimalInteger
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


whiteSpaceRules :: ScannerBuilder ()
whiteSpaceRules = undefined


anyCharCs :: CharSet
anyCharCs = undefined


graphicCharCs :: CharSet
graphicCharCs = EnumSet.unions
    [
        smallCharCs,
        largeCharCs,
        symbolCharCs
    ]

whiteCharCs :: CharSet
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
newlineP = undefined

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

smallCharCs :: CharSet
smallCharCs = EnumSet.unions
    [
        CodeUnit.catLowercaseLetter,
        CodeUnit.catOtherLetter,
        charsCs [
            '_'
        ]
    ]

largeCharCs :: CharSet
largeCharCs = EnumSet.unions
    [
        CodeUnit.catUppercaseLetter,
        CodeUnit.catTitlecaseLetter
    ]

symbolCharCs :: CharSet
symbolCharCs = EnumSet.unions
    [
        symbolCatCharCs
    ] `EnumSet.difference` EnumSet.unions [
        specialCharCs
    ]

symbolCatCharCs :: CharSet
symbolCatCharCs = EnumSet.unions
    [
        CodeUnit.catConnectorPunctuation,
        CodeUnit.catDashPunctuation,
        CodeUnit.catOtherPunctuation,
        CodeUnit.catSymbol
    ]

digitCharCs :: CharSet
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
        charsCs [
            '#',
            '"',
            '\''
        ]
    ]


charSetP :: CharSet -> Pattern
charSetP = Tlex.straightEnumSetP

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
