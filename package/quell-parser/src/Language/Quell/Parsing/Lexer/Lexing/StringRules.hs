{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Quell.Parsing.Lexer.Lexing.StringRules where

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
    = LexedStrSep
    | LexedInterpOpen
    | LexedInterpClose
    | LexedComponentChar
    | LexedComponentEsc Char
    | LexedComponentByteEsc
    | LexedComponentUniEsc
    | LexedComponentHexit Int
    | LexedComponentEnd

data LexerState
    = Start
    | Component
    | ComponentHexit
    deriving (Eq, Ord, Show, Enum)

type LexerCodeUnit = CodeUnit.T
type CharSet = EnumSet.EnumSet LexerCodeUnit

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit

startRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
startRule = TlexTH.thLexRule [Start]

componentRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
componentRule = TlexTH.thLexRule [Component]

componentHexitRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
componentHexitRule = TlexTH.thLexRule [ComponentHexit]

componentEscOpenRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
componentEscOpenRule p = componentRule do Rules.escapeOpenCharP <> p


lexerRules :: ScannerBuilder ()
lexerRules = do
    startRules
    componentRules

startRules :: ScannerBuilder ()
startRules = do
    startRule Rules.strSepCharP [||LexedStrSep||]
    startRule Rules.interpCloseP [||LexedInterpClose||]

componentRules :: ScannerBuilder ()
componentRules = do
    componentUniEscRules
    componentByteEscRules
    componentCharEscRules
    componentRule Rules.bstrGraphicCharP [||LexedComponentChar||]

    componentRule Rules.strSepCharP [||LexedStrSep||]
    componentRule Rules.interpOpenP [||LexedInterpOpen||]

componentUniEscRules :: ScannerBuilder ()
componentUniEscRules = do
    componentRule Rules.uniEscapePrefixP [||LexedComponentUniEsc||]

componentByteEscRules :: ScannerBuilder ()
componentByteEscRules = do
    componentRule Rules.byteEscapePrefixP [||LexedComponentByteEsc||]

componentCharEscRules :: ScannerBuilder ()
componentCharEscRules = do
    componentEscOpenRule (Rules.strP "0") [||LexedComponentEsc '\0'||]
    componentEscOpenRule (Rules.strP "a") [||LexedComponentEsc '\a'||]
    componentEscOpenRule (Rules.strP "b") [||LexedComponentEsc '\b'||]
    componentEscOpenRule (Rules.strP "f") [||LexedComponentEsc '\f'||]
    componentEscOpenRule (Rules.strP "n") [||LexedComponentEsc '\n'||]
    componentEscOpenRule (Rules.strP "r") [||LexedComponentEsc '\r'||]
    componentEscOpenRule (Rules.strP "t") [||LexedComponentEsc '\t'||]
    componentEscOpenRule (Rules.strP "v") [||LexedComponentEsc '\v'||]
    -- interp_string_sep_char
    componentEscOpenRule (Rules.strP "\"") [||LexedComponentEsc '"'||]
    -- escape_open_char
    componentEscOpenRule (Rules.strP "\\") [||LexedComponentEsc '\\'||]
    -- interp_open_char
    componentEscOpenRule (Rules.strP "#") [||LexedComponentEsc '#'||]

componentHexitRules :: ScannerBuilder ()
componentHexitRules = do
    componentHexitRule Rules.digit0P [||LexedComponentHexit 0x0||]
    componentHexitRule Rules.digit1P [||LexedComponentHexit 0x1||]
    componentHexitRule Rules.digit2P [||LexedComponentHexit 0x2||]
    componentHexitRule Rules.digit3P [||LexedComponentHexit 0x3||]
    componentHexitRule Rules.digit4P [||LexedComponentHexit 0x4||]
    componentHexitRule Rules.digit5P [||LexedComponentHexit 0x5||]
    componentHexitRule Rules.digit6P [||LexedComponentHexit 0x6||]
    componentHexitRule Rules.digit7P [||LexedComponentHexit 0x7||]
    componentHexitRule Rules.digit8P [||LexedComponentHexit 0x8||]
    componentHexitRule Rules.digit9P [||LexedComponentHexit 0x9||]
    componentHexitRule Rules.hexitAP [||LexedComponentHexit 0xA||]
    componentHexitRule Rules.hexitBP [||LexedComponentHexit 0xB||]
    componentHexitRule Rules.hexitCP [||LexedComponentHexit 0xC||]
    componentHexitRule Rules.hexitDP [||LexedComponentHexit 0xD||]
    componentHexitRule Rules.hexitEP [||LexedComponentHexit 0xE||]
    componentHexitRule Rules.hexitFP [||LexedComponentHexit 0xF||]
    componentHexitRule (Rules.strP "}") [||LexedComponentEnd||]
