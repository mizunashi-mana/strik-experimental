module Language.Quell.Pipeline.Source2Ast (
    Source (..),
    source2Program,
    source2Type,
    source2Expr,
    source2Tokens,
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Parsing.Lexer            as Lexer
import qualified Language.Quell.Parsing.Lexer.Encoding   as Encoding
import qualified Language.Quell.Parsing.Spanned          as Spanned
import qualified Language.Quell.Parsing.Parser           as Parser
import qualified Language.Quell.Parsing.Parser.Layout    as Layout
import qualified Language.Quell.Type.Ast                 as Ast
import qualified Language.Quell.Type.Token               as Token


type Pipeline i m f = Conduit.ConduitT i Conduit.Void m (Parser.Result f)

data Source i m = Source
    {
        sourceConduit  :: Conduit.ConduitT i ByteString m (),
        sourceEncoding :: Encoding.T
    }


source2Program :: Lexer.LexerMonad s m
    => Source i m -> Pipeline i m Ast.Program
source2Program s = source2Tokens s
    Conduit..| skipWsToken
    Conduit..| Layout.preParseForProgram
    Conduit..| Parser.parseProgram

source2Type :: Lexer.LexerMonad s m
    => Source i m -> Pipeline i m Ast.TypeExpr
source2Type s = source2Tokens s
    Conduit..| skipWsToken
    Conduit..| Layout.preParseForPart
    Conduit..| Parser.parseType

source2Expr :: Lexer.LexerMonad s m
    => Source i m -> Pipeline i m Ast.Expr
source2Expr s = source2Tokens s
    Conduit..| skipWsToken
    Conduit..| Layout.preParseForPart
    Conduit..| Parser.parseExpr

source2Tokens :: Lexer.LexerMonad s m
    => Source i m -> Conduit.ConduitT i (Spanned.T Token.T) m ()
source2Tokens s
    =           sourceConduit s
    Conduit..|  Lexer.lexerConduit do sourceEncoding s

skipWsToken :: Monad m
    => Conduit.ConduitT (Spanned.T Token.T) (Spanned.T Token.LexToken) m ()
skipWsToken = Conduit.await >>= \case
    Nothing ->
        pure ()
    Just st -> do
        case Spanned.unSpanned st of
            Token.TokLexeme t ->
                Conduit.yield do st <&> \_ -> t
            Token.TokWhiteSpace{} ->
                pure ()
        skipWsToken
