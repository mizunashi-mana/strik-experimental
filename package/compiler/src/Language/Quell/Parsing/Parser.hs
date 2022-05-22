{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.Quell.Parsing.Parser where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Parser.Ptera.TH                as Ptera
import qualified Language.Quell.Data.BufferedConduit     as BufferedConduit
import qualified Language.Quell.Data.Monad.MonadST       as MonadST
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Language.Quell.Parsing.Parser.Layout    as Layout
import qualified Language.Quell.Parsing.Parser.Rules     as Rules
import           Language.Quell.Parsing.Parser.RulesLib
import qualified Language.Quell.Type.Ast                 as Ast


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

newtype ParserT s m a = ParserT
    { unParserT :: BufferedConduit.T s Layout.TokenWithL Conduit.Void m a
    }
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadIO
    ) via BufferedConduit.T s Layout.TokenWithL Conduit.Void m

instance MonadST.T s m => MonadST.MonadST s (ParserT s m) where
    type Marker (ParserT s m) = MonadST.Marker m
    liftST mx = ParserT do MonadST.liftST mx

instance MonadST.T s m => Ptera.Scanner Int Layout.TokenWithL (ParserT s m) where
    consumeInput = ParserT BufferedConduit.await
    getPosMark = ParserT BufferedConduit.getCurrentPosition
    seekToPosMark i = ParserT do BufferedConduit.seekToPosition i
    scanMode = \case
        Ptera.ScanModeNoBack ->
            ParserT do BufferedConduit.setBufferMode BufferedConduit.NoBack
        Ptera.ScanModeNeedBack i ->
            ParserT do BufferedConduit.setBufferMode do BufferedConduit.NeedBack i

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

runParserT
    :: MonadST.T s m
    => ParserT s m (Ptera.Result Int (f AstParsed.T)) -> ParseConduit m f
runParserT mr = do
    r <- BufferedConduit.runConduitT do unParserT mr
    pure case r of
        Ptera.Parsed x ->
            Parsed x
        Ptera.ParseFailed{} ->
            ParseFailed

type ParseConduit m f = Conduit.ConduitT Layout.TokenWithL Conduit.Void m (Result f)

parseProgram :: MonadST.T s m => ParseConduit m Ast.Program
parseProgram = runParserT
    do Ptera.runParserM (Proxy :: Proxy "program EOS") pteraTHRunner ictx
    where
        ictx = GrammarContext
            { gctxLayoutStack = []
            }

parseType :: MonadST.T s m => ParseConduit m Ast.TypeExpr
parseType = runParserT
    do Ptera.runParserM (Proxy :: Proxy "type EOS") pteraTHRunner ictx
    where
        ictx = GrammarContext
            { gctxLayoutStack = []
            }

parseExpr :: MonadST.T s m => ParseConduit m Ast.Expr
parseExpr = runParserT
    do Ptera.runParserM (Proxy :: Proxy "expr EOS") pteraTHRunner ictx
    where
        ictx = GrammarContext
            { gctxLayoutStack = []
            }
