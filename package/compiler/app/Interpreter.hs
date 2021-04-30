module Main where

import           Language.Quell.Prelude

import qualified Language.Quell.Parsing.Lexer.Encoding as Encoding
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
        IO.putStr "> "
        IO.hFlush IO.stdout
        line <- ByteString.getLine
        IO.print line
        if
            | onull line -> IO.hIsEOF IO.stdin >>= \case
                True  -> pure ()
                False -> go
            | otherwise -> do
                let p = Source2Ast.source2Program do source line
                r <- Conduit.runConduit p
                IO.print r

