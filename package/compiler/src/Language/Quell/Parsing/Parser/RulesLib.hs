{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Parser.RulesLib (
    GrammarContext (..),
    LayoutItem (..),
    lexToken,
    unSpanned,
    layoutStack,
    interpStringLit,
    interpStringExpr,
) where

import           Language.Quell.Prelude

import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Type.Ast as Ast
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Language.Quell.Parsing.Parser.Layout    as Layout
import qualified Language.Parser.Ptera.TH.Class.LiftType as LiftType


data GrammarContext = GrammarContext
    { gctxLayoutStack :: [LayoutItem]
    }
    deriving (Eq, Show)

instance LiftType.LiftType GrammarContext where
    liftType _ = [t|GrammarContext|]

data LayoutItem
    = NoLayout
    | ImplicitLayout Layout.Position
    | ExplicitScopedLayout Layout.Position
    deriving (Eq, Show)

lexToken :: Layout.TokenWithL -> Spanned.Spanned Token.LexToken
lexToken = \case
    Layout.Token st ->
        st
    _ ->
        error "unreachable: expect lexed token, but actually a layout token is given."

unSpanned :: Spanned.Spanned a -> a
unSpanned x = Spanned.unSpanned x

layoutStack :: GrammarContext -> [LayoutItem]
layoutStack = gctxLayoutStack

interpStringLit :: Spanned.Spanned Token.LexToken -> Ast.InterpStringPart AstParsed.T
interpStringLit t = case Spanned.unSpanned t of
    Token.InterpStringWithoutInterp txt ->
        Ast.InterpStringLit txt do
            AstParsed.sp t
    Token.InterpStringStart txt ->
        Ast.InterpStringLit txt do
            AstParsed.sp t
    Token.InterpStringContinue txt ->
        Ast.InterpStringLit txt do
            AstParsed.sp t
    Token.InterpStringEnd txt ->
        Ast.InterpStringLit txt do
            AstParsed.sp t
    _ ->
        error "unreachable: expected interp string literal token, but actually the other token is given"

interpStringExpr :: Ast.Expr AstParsed.T -> Ast.InterpStringPart AstParsed.T
interpStringExpr e = Ast.InterpStringExpr e do AstParsed.sp e
