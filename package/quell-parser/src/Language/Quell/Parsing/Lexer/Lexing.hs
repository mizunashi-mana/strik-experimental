{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Lexing where

import Language.Quell.Prelude

import qualified Language.Quell.Parsing.Lexer.Rules as Rules
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Frontend.Data.Token as Token
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Quell.Data.Monad.MonadST as MonadST
import qualified Language.Lexer.Tlex as Tlex
import qualified Data.EnumSet as EnumSet
import qualified Language.Quell.Data.TextId as TextId
import qualified Language.Quell.Parsing.Lexer.Input as Input
import qualified Language.Quell.Parsing.Lexer.Lexing.KeywordLexing as KeywordLexing

$(Rules.buildLexer)

buildInputUnit :: Input.BuildInputUnit
buildInputUnit mLastItem (bs, c) = Spanned.Spanned
    { Spanned.unSpanned = (c, u)
    , Spanned.getSpan = sp
    }
    where
        u = CodeUnit.fromChar c

        newlineCRLF = [CodeUnit.LcGCarriageReturn, CodeUnit.LcGEndOfLine]
        isNewlineCodeUnit lcu = EnumSet.member lcu Rules.newlineCharCs

        sp = case mLastItem of
            Just lastItem -> do
                let lastLoc = Spanned.endLoc do Spanned.getSpan lastItem
                    (_, lastU) = Spanned.unSpanned lastItem
                    beginl = lastLoc
                        { Spanned.locBytesPos = beginlBytesPos
                        }
                    endl = if
                        | not do isNewlineCodeUnit u ->
                            beginl
                                { Spanned.locCol = Spanned.locCol beginl + 1
                                , Spanned.locBytesPos = endlBytesPos
                                }
                        -- "\r\n"
                        | [lastU, u] == newlineCRLF ->
                            beginl
                                { Spanned.locBytesPos = endlBytesPos
                                }
                        | otherwise ->
                            beginl
                                { Spanned.locLine = Spanned.locLine beginl + 1
                                , Spanned.locCol = 0
                                , Spanned.locBytesPos = endlBytesPos
                                }
                Spanned.Span
                    { Spanned.beginLoc = beginl
                    , Spanned.endLoc = endl
                    }
            Nothing -> do
                let beginl = Spanned.Loc
                        { Spanned.locLine = 0
                        , Spanned.locCol = 0
                        , Spanned.locBytesPos = beginlBytesPos
                        }
                    endl = if
                        | not do isNewlineCodeUnit u -> beginl
                            {
                                Spanned.locCol = 1,
                                Spanned.locBytesPos = endlBytesPos
                            }
                        | otherwise -> beginl
                            {
                                Spanned.locLine = 1,
                                Spanned.locBytesPos = endlBytesPos
                            }
                Spanned.Span
                    {
                        Spanned.beginLoc = beginl,
                        Spanned.endLoc = endl
                    }

        beginlBytesPos = Spanned.bytesIndex bs
        endlBytesPos = Spanned.bytesIndex bs + Spanned.bytesLength bs

lexer :: forall s m. MonadST.T s m => Input.Lexer s m ()
lexer = go Rules.Initial where
    go :: Rules.LexerState -> Input.Lexer s m ()
    go lst = do
        pos0 <- Input.getCurrentPosition
        Input.setNeedBackMode pos0
        tlexScan lst >>= \case
            Tlex.TlexEndOfInput -> do
                pure ()
            Tlex.TlexNotAccepted -> do
                Input.yieldTlexError
            Tlex.TlexAccepted pos1 act -> do
                lexAndYield pos0 pos1 act
                go Rules.Initial

    lexAndYield pos0 pos1 = \case
        Rules.WithWhiteSpace -> do
            -- The next mode change clean the buffer.
            Input.seekToPosition pos1
        Rules.WithToken t -> do
            yieldToken pos0 pos1 t
        Rules.WithIdType t -> do
            yieldIdType pos0 pos1 t
        Rules.WithKwToken -> do
            yieldKwToken pos0 pos1
        Rules.LexIdFreeIdStart -> do
            lexAndYieldFreeId pos0 pos1
        Rules.LexLitRationalWithDot -> do
            lexAndYieldLitRationalWithDot pos0 pos1
        Rules.LexLitDefaultInteger -> do
            lexAndYieldLitDefaultInteger pos0 pos1
        Rules.LexLitDecimalInteger -> do
            lexAndYieldLitDecimalInteger pos0 pos1
        Rules.LexLitHeximalInteger -> do
            lexAndYieldLitHeximalInteger pos0 pos1
        Rules.LexInterpStringStart -> do
            lexAndYieldInterpStringStart pos0 pos1
        Rules.LexInterpStringContinue -> do
            lexAndYieldInterpStringContinue pos0 pos1
        Rules.LexCommentLineWithContent -> do
            lexAndYieldCommentLineWithContent pos0 pos1
        Rules.LexCommentMultilineWithContent -> do
            lexAndYieldCommentMultilineWithContent pos0 pos1

yieldToken :: MonadST.T s m => Int -> Int -> Token.T -> Input.Lexer s m ()
yieldToken pos0 pos1 tok = do
    sp0To1 <- Input.lexSpan pos0 pos1
    let u = Spanned.Spanned
            { Spanned.getSpan = sp0To1
            , Spanned.unSpanned = Input.LexedToken tok
            }
    Input.lexerYield u

yieldIdType :: MonadST.T s m => Int -> Int -> Rules.IdType -> Input.Lexer s m ()
yieldIdType pos0 pos1 (Rules.IdType t) = do
    spannedTxtBuilder0To1 <- Input.consumeLexedUnitsAndSwitchToNoBackMode
        do \item -> item <&> \(c, _) -> textBuilderFromChar c
        do \spannedTxtBuilder item -> do
            let (c, _) = Spanned.unSpanned item
            Spanned.Spanned
                { Spanned.getSpan =
                    Spanned.getSpan spannedTxtBuilder <> Spanned.getSpan item
                , Spanned.unSpanned =
                    Spanned.unSpanned spannedTxtBuilder <> textBuilderFromChar c
                }
        pos0 pos1
    let u = spannedTxtBuilder0To1 <&> \txtBuilder -> Input.LexedToken do
            Token.TokLexeme do t do TextId.textId do buildText txtBuilder
    Input.lexerYield u

yieldKwToken :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
yieldKwToken pos0 pos1 = KeywordLexing.lexer pos1 >>= \case
    Just tok -> goAccepted tok
    Nothing -> Input.yieldTlexError
    where
        goAccepted tok = do
            sp0To1 <- Input.lexSpan pos0 pos1
            let u = Spanned.Spanned
                    { Spanned.getSpan = sp0To1
                    , Spanned.unSpanned = Input.LexedToken do Token.TokLexeme tok
                    }
            Input.lexerYield u

lexAndYieldFreeId :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldFreeId = undefined

lexAndYieldLitRationalWithDot :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldLitRationalWithDot = undefined

lexAndYieldLitDefaultInteger :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldLitDefaultInteger = undefined

lexAndYieldLitDecimalInteger :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldLitDecimalInteger = undefined

lexAndYieldLitHeximalInteger :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldLitHeximalInteger = undefined

lexAndYieldInterpStringStart :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldInterpStringStart = undefined

lexAndYieldInterpStringContinue :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldInterpStringContinue = undefined

lexAndYieldCommentLineWithContent :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldCommentLineWithContent = undefined

lexAndYieldCommentMultilineWithContent :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldCommentMultilineWithContent = undefined
