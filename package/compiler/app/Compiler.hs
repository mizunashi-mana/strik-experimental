module Main where

import           Language.Quell.Prelude

import qualified Language.Quell.Parsing.Lexer.Encoding as Encoding
import qualified Language.Quell.Parsing.Parser.AstParsed as AstParsed
import qualified Language.Quell.Parsing.Parser.Runner as Runner
import qualified Language.Quell.Parsing.Parser.Error as Error
import qualified Language.Quell.Parsing.Parser.Layout as Layout
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Type.Ast as Ast
import qualified Language.Quell.Type.Token as Token
import qualified Language.Quell.Pipeline.Source2Ast as Source2Ast
import qualified Conduit
import qualified System.IO as IO
import qualified Data.ByteString as ByteString


source :: ByteString -> Source2Ast.Source i IO
source bs = Source2Ast.Source
    {
        Source2Ast.sourceConduit = Conduit.yield bs,
        Source2Ast.sourceEncoding = Encoding.EncodingUtf8
    }

main :: IO ()
main = go where
    go = do
        input <- ByteString.getContents
        ts1 <- lex input
        IO.putStr "tokens: "
        IO.print ts1
        ts2 <- lexWithPreParse input
        IO.putStr "tokens for preParse: "
        IO.print ts2
        r <- parse input
        IO.putStr "parse result: "
        IO.print r

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
    | ParseFailed [Spanned.T Error.T]
    deriving (Eq, Show)

parse :: ByteString -> IO (ParseResult AstParsed.T)
parse line =
    parseProg \esProg -> IO.putStr "esProg: " >> IO.print esProg >>
    let es = esProg in pure do ParseFailed es
    where
        parseProg cont = do
            let p = Source2Ast.source2Program do source line
            r <- Conduit.runConduit p
            case r of
                Runner.RunnerSuccess x -> pure do ParseProgramSuccess x
                Runner.RunnerFailed es -> cont es
