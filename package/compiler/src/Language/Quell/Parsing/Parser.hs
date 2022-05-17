{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.Quell.Parsing.Parser where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Language.Quell.Parsing.Parser.Layout    as Layout
import qualified Language.Parser.Ptera.TH                as Ptera
import qualified Language.Quell.Type.Ast                 as Ast
import qualified Language.Quell.Parsing.Parser.Rules     as Rules
import           Language.Quell.Parsing.Parser.RulesLib


$(Ptera.genRunner
    (Ptera.GenParam
        {
            Ptera.startsTy = [t|Rules.ParsePoints|],
            Ptera.rulesTy  = [t|Rules.RuleDefs|],
            Ptera.tokensTy = [t|Rules.Tokens|],
            Ptera.tokenTy  = [t|Layout.TokenWithL|],
            Ptera.customCtxTy = [t|GrammarContext|]
        })
    Rules.grammar
    )

data ParseResult a
    = Parsed a
    | ParseFailed
    deriving (Eq, Show, Functor)

instance Applicative ParseResult where
    pure x = Parsed x

    mf <*> mx = case mf of
        Parsed f -> case mx of
            Parsed x ->
                Parsed do f x
            ParseFailed ->
                ParseFailed
        ParseFailed ->
            ParseFailed

instance Monad ParseResult where
    mx >>= f = case mx of
        Parsed x ->
            f x
        ParseFailed ->
            ParseFailed

type Result f = ParseResult (f AstParsed.T)

type ParseConduit m f = Conduit.ConduitT Layout.TokenWithL Conduit.Void m (Result f)

parseProgram :: ParseConduit m Ast.Program
parseProgram = undefined

parseType :: ParseConduit m Ast.TypeExpr
parseType = undefined

parseExpr :: ParseConduit m Ast.Expr
parseExpr = undefined
