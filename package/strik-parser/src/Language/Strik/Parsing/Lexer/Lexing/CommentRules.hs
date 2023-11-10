{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Strik.Parsing.Lexer.Lexing.CommentRules where

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
    = LexedClose
    | LexedComponentChar

data LexerState
    = LineComponent
    | MultilineComponent
    deriving (Eq, Ord, Show, Enum)

type LexerCodeUnit = CodeUnit.T
type CharSet = EnumSet.EnumSet LexerCodeUnit

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit

lineComponentRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
lineComponentRule = TlexTH.thLexRule [LineComponent]

multilineComponentRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
multilineComponentRule = TlexTH.thLexRule [MultilineComponent]


lexerRules :: ScannerBuilder ()
lexerRules = do
    componentRules

componentRules :: ScannerBuilder ()
componentRules = do
    lineComponentRule Rules.newlineP [||LexedClose||]
    lineComponentRule Rules.any1lCharP [||LexedComponentChar||]

    multilineComponentRule Rules.multilineCommentCloseP [||LexedClose||]
    -- Represented 'anys' as the priority of rules.
    multilineComponentRule Rules.anyCharP [||LexedComponentChar||]
