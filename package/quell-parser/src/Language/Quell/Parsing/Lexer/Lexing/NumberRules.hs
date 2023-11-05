{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Quell.Parsing.Lexer.Lexing.NumberRules where

import           Language.Quell.Prelude

import qualified Data.EnumSet                          as EnumSet
import qualified Language.Haskell.TH                   as TH
import qualified Language.Lexer.Tlex                   as Tlex
import qualified Language.Lexer.Tlex.Plugin.TH         as TlexTH
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Quell.Parsing.Lexer.Rules    as Rules

buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    stateTy <- [t|LexerState|]
    codeUnitTy <- [t|LexerCodeUnit|]
    actionTy <- [t|LexerAction|]
    let lexer = TlexTH.buildTHScanner codeUnitTy stateTy actionTy lexerRules
    TlexTH.outputScanner lexer


data LexerAction
    = LexedSignPositive
    | LexedSignNegative
    | LexedBase Integer
    | LexedComponentElement Integer
    | LexedDot
    | LexedSep

data LexerState
    = Sign
    | Base
    | Component
    deriving (Eq, Ord, Show, Enum)

type LexerCodeUnit = CodeUnit.T
type CharSet = EnumSet.EnumSet LexerCodeUnit

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit

signRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
signRule = TlexTH.thLexRule [Sign]

baseRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
baseRule = TlexTH.thLexRule [Base]

componentRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
componentRule = TlexTH.thLexRule [Component]


lexerRules :: ScannerBuilder ()
lexerRules = do
    signRules
    baseRules
    componentRules

signRules :: ScannerBuilder ()
signRules = do
    signRule (Rules.charsP ['+']) [||LexedSignPositive||]
    signRule (Rules.charsP ['-']) [||LexedSignNegative||]
    signRule mempty [||LexedSignPositive||]

baseRules :: ScannerBuilder ()
baseRules = do
    baseRule (Rules.zeroCharP <> Rules.charsP ['x', 'X']) [||LexedBase 0x10||]
    baseRule (Rules.zeroCharP <> Rules.charsP ['d', 'D']) [||LexedBase 10||]
    baseRule mempty [||LexedBase 10||]

componentRules :: ScannerBuilder ()
componentRules = do
    componentRule digit0P [||LexedComponentElement 0||]
    componentRule digit1P [||LexedComponentElement 1||]
    componentRule digit2P [||LexedComponentElement 2||]
    componentRule digit3P [||LexedComponentElement 3||]
    componentRule digit4P [||LexedComponentElement 4||]
    componentRule digit5P [||LexedComponentElement 5||]
    componentRule digit6P [||LexedComponentElement 6||]
    componentRule digit7P [||LexedComponentElement 7||]
    componentRule digit8P [||LexedComponentElement 8||]
    componentRule digit9P [||LexedComponentElement 9||]
    componentRule hexit10P [||LexedComponentElement 10||]
    componentRule hexit11P [||LexedComponentElement 11||]
    componentRule hexit12P [||LexedComponentElement 12||]
    componentRule hexit13P [||LexedComponentElement 13||]
    componentRule hexit14P [||LexedComponentElement 14||]
    componentRule hexit15P [||LexedComponentElement 15||]
    componentRule Rules.numSepSymCharP [||LexedSep||]
    componentRule Rules.numDotSymCharP [||LexedDot||]

digit0P = Rules.charSetP CodeUnit.groupDigit0
digit1P = Rules.charSetP CodeUnit.groupDigit1
digit2P = Rules.charSetP CodeUnit.groupDigit2
digit3P = Rules.charSetP CodeUnit.groupDigit3
digit4P = Rules.charSetP CodeUnit.groupDigit4
digit5P = Rules.charSetP CodeUnit.groupDigit5
digit6P = Rules.charSetP CodeUnit.groupDigit6
digit7P = Rules.charSetP CodeUnit.groupDigit7
digit8P = Rules.charSetP CodeUnit.groupDigit8
digit9P = Rules.charSetP CodeUnit.groupDigit9
hexit10P = Rules.charsP ['a', 'A']
hexit11P = Rules.charsP ['b', 'B']
hexit12P = Rules.charsP ['c', 'C']
hexit13P = Rules.charsP ['d', 'D']
hexit14P = Rules.charsP ['e', 'E']
hexit15P = Rules.charsP ['f', 'F']
