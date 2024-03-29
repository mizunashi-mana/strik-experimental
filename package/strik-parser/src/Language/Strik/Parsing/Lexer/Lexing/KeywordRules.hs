{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Strik.Parsing.Lexer.Lexing.KeywordRules where

import           Language.Strik.Prelude

import qualified Data.EnumSet                          as EnumSet
import qualified Language.Haskell.TH                   as TH
import qualified Language.Lexer.Tlex                   as Tlex
import qualified Language.Lexer.Tlex.Plugin.TH         as TlexTH
import qualified Language.Strik.Frontend.Data.Token    as Token
import qualified Language.Strik.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Strik.Parsing.Lexer.Rules    as Rules

buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    stateTy <- [t|LexerState|]
    codeUnitTy <- [t|LexerCodeUnit|]
    actionTy <- [t|LexerAction|]
    let lexer = TlexTH.buildTHScanner codeUnitTy stateTy actionTy lexerRules
    TlexTH.outputScanner lexer


type LexerAction = Token.LexToken

data LexerState = Initial
    deriving (Eq, Ord, Show, Enum)

type LexerCodeUnit = CodeUnit.T
type CharSet = EnumSet.EnumSet LexerCodeUnit

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit

initialRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
initialRule = TlexTH.thLexRule [Initial]

keywordPrefixedRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
keywordPrefixedRule p = initialRule do Rules.keywordPrefixCharP <> p


lexerRules :: ScannerBuilder ()
lexerRules = do
    keywordPrefixedRules
    keywordIdRules
    keywordSymRules

keywordPrefixedRules :: ScannerBuilder ()
keywordPrefixedRules = pure ()

keywordIdRules :: ScannerBuilder ()
keywordIdRules = do
    initialRule (Rules.strP "_") [||Token.KwUnderscore||]

keywordSymRules :: ScannerBuilder ()
keywordSymRules = do
    initialRule (Rules.strP "=") [||Token.KwSymEqual||]
    initialRule (Rules.strP "^") [||Token.KwSymCaret||]
    initialRule (Rules.strP ":") [||Token.KwSymColon||]
    initialRule (Rules.strP "\\") [||Token.KwSymBackslash||]
