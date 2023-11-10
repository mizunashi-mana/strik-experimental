{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Strik.Parsing.Lexer.Lexing.NumberRules where

import           Language.Strik.Prelude

import qualified Data.EnumSet                          as EnumSet
import qualified Language.Haskell.TH                   as TH
import qualified Language.Lexer.Tlex                   as Tlex
import qualified Language.Lexer.Tlex.Plugin.TH         as TlexTH
import qualified Language.Strik.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Strik.Parsing.Lexer.Rules    as Rules

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
    componentRule Rules.digit0P [||LexedComponentElement 0x0||]
    componentRule Rules.digit1P [||LexedComponentElement 0x1||]
    componentRule Rules.digit2P [||LexedComponentElement 0x2||]
    componentRule Rules.digit3P [||LexedComponentElement 0x3||]
    componentRule Rules.digit4P [||LexedComponentElement 0x4||]
    componentRule Rules.digit5P [||LexedComponentElement 0x5||]
    componentRule Rules.digit6P [||LexedComponentElement 0x6||]
    componentRule Rules.digit7P [||LexedComponentElement 0x7||]
    componentRule Rules.digit8P [||LexedComponentElement 0x8||]
    componentRule Rules.digit9P [||LexedComponentElement 0x9||]
    componentRule Rules.hexitAP [||LexedComponentElement 0xA||]
    componentRule Rules.hexitBP [||LexedComponentElement 0xB||]
    componentRule Rules.hexitCP [||LexedComponentElement 0xC||]
    componentRule Rules.hexitDP [||LexedComponentElement 0xD||]
    componentRule Rules.hexitEP [||LexedComponentElement 0xE||]
    componentRule Rules.hexitFP [||LexedComponentElement 0xF||]
    componentRule Rules.numSepSymCharP [||LexedSep||]
    componentRule Rules.numDotSymCharP [||LexedDot||]
