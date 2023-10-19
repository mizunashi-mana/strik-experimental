{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Lexer.Lexing where

import qualified Language.Quell.Lexer.Rules as Rules

$(Rules.buildLexer)
{-
data LexedUnit
    = LexedToken Token.T
    | LexError Error.T Text
    deriving (Eq, Show)

type LexerInput = (Spanned.BytesSpan, Char)
type LexerInputUnit = Spanned.T (Char, CodeUnit.T)
type LexerOutput = Spanned.T LexedUnit

runLexer :: MonadST.T s m => Lexer s m () -> Conduit.ConduitT LexerInput LexerOutput m ()
runLexer m = buildInputUnits
    Conduit..| BufferedConduit.runConduitT do unLexer m

lexer :: forall s m. MonadST.T s m => Lexer s m ()
lexer = go Rules.Initial where
    go :: Rules.LexerState -> Lexer s m ()
    go lst = do
        pos0 <- getCurrentPosition
        setNeedBackMode pos0
        tlexScan lst >>= \case
            Tlex.TlexEndOfInput -> do
                pure ()
            Tlex.TlexError -> do
                yieldTlexError
            Tlex.TlexAccepted pos1 act -> do
                case act of
                    Rules.WithToken t -> do
                        yieldToken pos0 pos1 t
                    Rules.WithIdToken t -> do
                        yieldIdToken pos0 pos1 t
                    Rules.WithWhitespace -> do
                        -- The next mode change clean the buffer.
                        seekToPosition pos1
                    Rules.LexLitRationalWithDot -> do
                        lexAndYieldLitRationalWithDot pos0 pos1
                    Rules.LexLitRationalWithoutDot ->  do
                        lexAndYieldLitRationalWithoutDot pos0 pos1
                    Rules.LexLitBitInteger -> do
                        lexAndYieldLitBitInteger pos0 pos1
                    Rules.LexLitOctitInteger -> do
                        lexAndYieldLitOctitInteger pos0 pos1
                    Rules.LexLitHexitInteger -> do
                        lexAndYieldLitHexitInteger pos0 pos1
                    Rules.LexLitDecimalInteger -> do
                        lexAndYieldLitDecimalInteger pos0 pos1
                    Rules.LexLitByteString -> do
                        lexAndYieldLitByteString pos0 pos1
                    Rules.LexLitByteChar -> do
                        lexAndYieldLitByteChar pos0 pos1
                    Rules.LexLitString -> do
                        lexAndYieldLitString pos0 pos1
                    Rules.LexLitChar -> do
                        lexAndYieldLitChar pos0 pos1
                    Rules.LexInterpStringStart -> do
                        lexAndYieldInterpStringStart pos0 pos1
                    Rules.LexInterpStringContinue -> do
                        lexAndYieldInterpStringContinue pos0 pos1
                    Rules.LexCommentLineWithContent -> do
                        lexAndYieldCommentLineWithContent pos0 pos1
                    Rules.LexCommentMultilineWithContent -> do
                        lexAndYieldCommentMultilineWithContent pos0 pos1
                    Rules.LexCommentDoc -> do
                        lexAndYieldCommentDoc pos0 pos1
                    Rules.LexCommentPragma -> do
                        lexAndYieldCommentPragma pos0 pos1
                go Rules.Initial
-}
