module Language.Quell.Parsing.Parser where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Language.Quell.Parsing.Parser.Layout    as Layout
import qualified Language.Quell.Type.Ast                 as Ast

data ParseResult a
    = Parsed a
    | ParseFailed
    deriving (Eq, Show, Functor)

type Result f = ParseResult (f AstParsed.T)

type ParseConduit m f = Conduit.ConduitT Layout.TokenWithL Conduit.Void m (Result f)

parseProgram :: Monad m => ParseConduit m Ast.Program
parseProgram = undefined

parseType :: Monad m => ParseConduit m Ast.TypeExpr
parseType = undefined

parseExpr :: Monad m => ParseConduit m Ast.Expr
parseExpr = undefined
