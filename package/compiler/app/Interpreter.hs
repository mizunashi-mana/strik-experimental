module Main where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Data.ByteString                         as ByteString
import qualified Language.Quell.Parsing.Lexer.Encoding   as Encoding
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Language.Quell.Parsing.Parser.Error     as Error
import qualified Language.Quell.Parsing.Parser.Layout    as Layout
import qualified Language.Quell.Parsing.Parser.Runner    as Runner
import qualified Language.Quell.Parsing.Spanned          as Spanned
import qualified Language.Quell.Pipeline.Source2Ast      as Source2Ast
import qualified Language.Quell.Type.Ast                 as Ast
import qualified Language.Quell.Type.Token               as Token
import qualified System.IO                               as IO


source :: ByteString -> Source2Ast.Source i IO
source bs = Source2Ast.Source
    {
        Source2Ast.sourceConduit = Conduit.yield bs,
        Source2Ast.sourceEncoding = Encoding.EncodingUtf8
    }

main :: IO ()
main = go where
    go = do
        IO.putStr "> "
        IO.hFlush IO.stdout
        line <- ByteString.getLine
        IO.print line
        ts1 <- lex line
        IO.putStr "tokens: "
        IO.print ts1
        ts2 <- lexWithPreParse line
        IO.putStr "tokens for preParse: "
        IO.print ts2
        r <- parse line
        IO.putStr "parse result: "
        IO.print r
        go

lex :: ByteString -> IO [Spanned.T Token.T]
lex line = do
    let p = Source2Ast.source2Tokens do source line
    Conduit.runConduit do p Conduit..| Conduit.sinkList

lexWithPreParse :: ByteString -> IO [Layout.TokenWithL]
lexWithPreParse line = do
    let p = Source2Ast.source2Tokens do source line
    Conduit.runConduit do
        p
            Conduit..| Layout.preParseForProgram
            Conduit..| Conduit.sinkList

data ParseResult c
    = ParseProgramSuccess (Ast.Program c)
    | ParseExprSuccess (Ast.Expr c)
    | ParseTypeSuccess (Ast.TypeExpr c)
    | ParseFailed [Spanned.T Error.T]
    deriving (Eq, Show)

parse :: ByteString -> IO (ParseResult AstParsed.T)
parse line =
    parseProg \esProg -> IO.putStr "esProg: " >> IO.print esProg >>
    parseExpr \esExpr -> IO.putStr "esExpr: " >> IO.print esExpr >>
    parseType \esType -> IO.putStr "esType: " >> IO.print esType >>
    let es = esType in pure do ParseFailed es
    where
        parseProg cont = do
            let p = Source2Ast.source2Program do source line
            r <- Conduit.runConduit p
            case r of
                Runner.RunnerSuccess x -> pure do ParseProgramSuccess x
                Runner.RunnerFailed es -> cont es

        parseExpr cont = do
            let p = Source2Ast.source2Expr do source line
            r <- Conduit.runConduit p
            case r of
                Runner.RunnerSuccess x -> pure do ParseExprSuccess x
                Runner.RunnerFailed es -> cont es

        parseType cont = do
            let p = Source2Ast.source2Type do source line
            r <- Conduit.runConduit p
            case r of
                Runner.RunnerSuccess x -> pure do ParseTypeSuccess x
                Runner.RunnerFailed es -> cont es

parseCustom :: Runner.Runner IO a -> ByteString -> IO (Runner.RunnerResult a)
parseCustom r line = do
    let p = Source2Ast.source2Tokens do source line
            Conduit..| Layout.preParse
            Conduit..| Runner.runRunner r
    Conduit.runConduit p
