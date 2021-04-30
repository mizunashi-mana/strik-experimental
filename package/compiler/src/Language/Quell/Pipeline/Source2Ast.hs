module Language.Quell.Pipeline.Source2Ast (
    Source (..),
    source2Program,
    source2Expr,
    source2ParseResult,
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Parsing.Lexer            as Lexer
import qualified Language.Quell.Parsing.Lexer.Encoding   as Encoding
import qualified Language.Quell.Parsing.Parser           as Parser
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Language.Quell.Parsing.Parser.Layout    as Layout
import qualified Language.Quell.Parsing.Parser.Runner    as Runner
import qualified Language.Quell.Type.Ast                 as Ast


type ParseResult f = Runner.RunnerResult (f AstParsed.T)
type ParseConduit i m f = Conduit.ConduitT i Conduit.Void m (ParseResult f)

data Source i m = Source
    {
        sourceConduit  :: Conduit.ConduitT i ByteString m (),
        sourceEncoding :: Encoding.T
    }


source2Program :: Lexer.LexerMonad s m
    => Source i m -> ParseConduit i m Ast.Program
source2Program = source2ParseResult Parser.parseProgram

source2Expr :: Lexer.LexerMonad s m
    => Source i m -> ParseConduit i m Ast.Expr
source2Expr = source2ParseResult Parser.parseExpr

source2ParseResult :: Lexer.LexerMonad s m
    => Runner.Runner m a -> Source i m
    -> Conduit.ConduitT i Conduit.Void m (Runner.RunnerResult a)
source2ParseResult r s
    =           sourceConduit s
    Conduit..|  Lexer.lexerConduit do sourceEncoding s
    Conduit..|  Layout.preParse
    Conduit..|  Runner.runRunner r
