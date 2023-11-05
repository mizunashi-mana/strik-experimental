{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Lexing where

import           Language.Quell.Prelude

import qualified Data.EnumSet                                      as EnumSet
import qualified Language.Lexer.Tlex                               as Tlex
import qualified Language.Quell.Data.Monad.MonadST                 as MonadST
import qualified Language.Quell.Data.TextId                        as TextId
import qualified Language.Quell.Frontend.Data.Token                as Token
import qualified Language.Quell.Parsing.Lexer.CodeUnit             as CodeUnit
import qualified Language.Quell.Parsing.Lexer.Input                as Input
import qualified Language.Quell.Parsing.Lexer.Lexing.KeywordLexing as KeywordLexing
import qualified Language.Quell.Parsing.Lexer.Lexing.NumberRules  as NumberRules
import qualified Language.Quell.Parsing.Lexer.Lexing.NumberLexing  as NumberLexing
import qualified Language.Quell.Parsing.Lexer.Rules                as Rules
import qualified Language.Quell.Parsing.Spanned                    as Spanned

$(Rules.buildLexer)

buildInputUnit :: Input.BuildInputUnit
buildInputUnit mLastItem (bs, c) = Spanned.Spanned
    { Spanned.unSpanned = (c, u)
    , Spanned.getSpan = sp
    }
    where
        u = CodeUnit.fromChar c

        newlineCRLF = [CodeUnit.LcACarriageReturn, CodeUnit.LcAEndOfLine]
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
        Rules.LexIdFreeId -> do
            lexAndYieldFreeId pos0 pos1
        Rules.LexLitString -> do
            lexAndYieldLitString pos0 pos1
        Rules.LexLitRational -> do
            lexAndYieldLitRational pos0 pos1
        Rules.LexLitInteger -> do
            lexAndYieldLitInteger pos0 pos1
        Rules.LexInterpStringStart -> do
            lexAndYieldInterpStringStart pos0 pos1
        Rules.LexInterpStringContinue -> do
            lexAndYieldInterpStringContinue pos0 pos1
        Rules.LexInterpStringEnd -> do
            lexAndYieldInterpStringEnd pos0 pos1
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

data LexItemStateOfRational = LexItemStateOfRational
    {
        lexItemStateOfRationalSign     :: Bool,
        lexItemStateOfRationalBase     :: Integer,
        lexItemStateOfRationalFraction :: Integer,
        lexItemStateOfRationalExponent :: Integer
    }

lexAndYieldLitRational :: forall s m. MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldLitRational posStart posEnd = do
        Input.setNeedBackMode posStart
        goSign
        Input.seekToPosition posEnd
    where
        unexpectedAction = error "unreachable: unexpected action"

        goSign :: Input.Lexer s m ()
        goSign = NumberLexing.tlexScan NumberRules.Sign >>= \case
            Tlex.TlexEndOfInput -> do
                Input.yieldTlexError
            Tlex.TlexNotAccepted -> do
                Input.yieldTlexError
            Tlex.TlexAccepted pos1 act -> do
                let sign = case act of
                        NumberRules.LexedSignPositive -> True
                        NumberRules.LexedSignNegative -> False
                        NumberRules.LexedBase{} -> unexpectedAction
                        NumberRules.LexedComponentElement{} -> unexpectedAction
                        NumberRules.LexedDot -> unexpectedAction
                        NumberRules.LexedSep -> unexpectedAction
                Input.seekToPosition pos1
                goBase sign

        goBase :: Bool -> Input.Lexer s m ()
        goBase sign = NumberLexing.tlexScan NumberRules.Base >>= \case
            Tlex.TlexEndOfInput -> do
                Input.yieldTlexError
            Tlex.TlexNotAccepted -> do
                Input.yieldTlexError
            Tlex.TlexAccepted pos1 act -> do
                Input.seekToPosition pos1
                let base = case act of
                        NumberRules.LexedBase e -> e
                        NumberRules.LexedSignPositive -> unexpectedAction
                        NumberRules.LexedSignNegative -> unexpectedAction
                        NumberRules.LexedComponentElement{} -> unexpectedAction
                        NumberRules.LexedDot -> unexpectedAction
                        NumberRules.LexedSep -> unexpectedAction
                let s = LexItemStateOfRational
                        {
                            lexItemStateOfRationalSign = sign,
                            lexItemStateOfRationalBase = base,
                            lexItemStateOfRationalFraction = 0,
                            lexItemStateOfRationalExponent = 0
                        }
                goComponent1 s

        goComponent1 :: LexItemStateOfRational -> Input.Lexer s m ()
        goComponent1 s0 = NumberLexing.tlexScan NumberRules.Component >>= \case
            Tlex.TlexEndOfInput -> do
                Input.yieldTlexError
            Tlex.TlexNotAccepted -> do
                Input.yieldTlexError
            Tlex.TlexAccepted pos1 act -> do
                Input.seekToPosition pos1
                case act of
                    NumberRules.LexedComponentElement e -> do
                        let base = lexItemStateOfRationalBase s0
                            fraction0 = lexItemStateOfRationalFraction s0
                            s1 = s0
                                {
                                    lexItemStateOfRationalFraction = fraction0 * base + e
                                }
                        goComponent1 s1
                    NumberRules.LexedSep -> goComponent1 s0
                    NumberRules.LexedDot -> goComponent2 s0
                    NumberRules.LexedSignPositive -> unexpectedAction
                    NumberRules.LexedSignNegative -> unexpectedAction
                    NumberRules.LexedBase{} -> unexpectedAction

        goComponent2 :: LexItemStateOfRational -> Input.Lexer s m ()
        goComponent2 s0 = NumberLexing.tlexScan NumberRules.Component >>= \case
            Tlex.TlexEndOfInput -> do
                Input.yieldTlexError
            Tlex.TlexNotAccepted -> do
                Input.yieldTlexError
            Tlex.TlexAccepted pos1 act -> do
                Input.seekToPosition pos1
                let base = lexItemStateOfRationalBase s0
                    fraction0 = lexItemStateOfRationalFraction s0
                    exponent0 = lexItemStateOfRationalExponent s0
                    s1 = case act of
                        NumberRules.LexedComponentElement e -> s0
                            {
                                lexItemStateOfRationalFraction = fraction0 * base + e,
                                lexItemStateOfRationalExponent = exponent0 + 1
                            }
                        NumberRules.LexedSep -> s0
                        NumberRules.LexedDot -> unexpectedAction
                        NumberRules.LexedSignPositive -> unexpectedAction
                        NumberRules.LexedSignNegative -> unexpectedAction
                        NumberRules.LexedBase{} -> unexpectedAction
                if
                    | pos1 < posEnd -> goComponent2 s1
                    | otherwise -> yieldLitRationalToken s1

        yieldLitRationalToken :: LexItemStateOfRational -> Input.Lexer s m ()
        yieldLitRationalToken s = do
            let base = lexItemStateOfRationalBase s
                fraction0 = case lexItemStateOfRationalSign s of
                    True -> lexItemStateOfRationalFraction s
                    False -> negate do lexItemStateOfRationalFraction s
                exponent0 = lexItemStateOfRationalExponent s
                tok = Input.LexedToken do
                    Token.TokLexeme do
                        Token.LitRational if
                            | exponent0 < 0 -> fraction0 % base ^ exponent0
                            | otherwise    -> fraction0 * base ^ exponent0 % 1
            span0 <- Input.lexSpan posStart posEnd
            Input.lexerYield do
                Spanned.Spanned
                    {
                        Spanned.getSpan = span0,
                        Spanned.unSpanned = tok
                    }

data LexItemStateOfInteger = LexItemStateOfInteger
    {
        lexItemStateOfIntegerSign     :: Bool,
        lexItemStateOfIntegerBase     :: Integer,
        lexItemStateOfIntegerFraction :: Integer
    }

lexAndYieldLitInteger :: forall s m. MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldLitInteger posStart posEnd = do
        Input.setNeedBackMode posStart
        goSign
        Input.seekToPosition posEnd
    where
        unexpectedAction = error "unreachable: unexpected action"

        goSign :: Input.Lexer s m ()
        goSign = NumberLexing.tlexScan NumberRules.Sign >>= \case
            Tlex.TlexEndOfInput -> do
                Input.yieldTlexError
            Tlex.TlexNotAccepted -> do
                Input.yieldTlexError
            Tlex.TlexAccepted pos1 act -> do
                let sign = case act of
                        NumberRules.LexedSignPositive -> True
                        NumberRules.LexedSignNegative -> False
                        NumberRules.LexedBase{} -> unexpectedAction
                        NumberRules.LexedComponentElement{} -> unexpectedAction
                        NumberRules.LexedDot -> unexpectedAction
                        NumberRules.LexedSep -> unexpectedAction
                Input.seekToPosition pos1
                goBase sign

        goBase :: Bool -> Input.Lexer s m ()
        goBase sign = NumberLexing.tlexScan NumberRules.Base >>= \case
            Tlex.TlexEndOfInput -> do
                Input.yieldTlexError
            Tlex.TlexNotAccepted -> do
                Input.yieldTlexError
            Tlex.TlexAccepted pos1 act -> do
                Input.seekToPosition pos1
                let base = case act of
                        NumberRules.LexedBase e -> e
                        NumberRules.LexedSignPositive -> unexpectedAction
                        NumberRules.LexedSignNegative -> unexpectedAction
                        NumberRules.LexedComponentElement{} -> unexpectedAction
                        NumberRules.LexedDot -> unexpectedAction
                        NumberRules.LexedSep -> unexpectedAction
                let s = LexItemStateOfInteger
                        {
                            lexItemStateOfIntegerSign = sign,
                            lexItemStateOfIntegerBase = base,
                            lexItemStateOfIntegerFraction = 0
                        }
                goComponent s

        goComponent :: LexItemStateOfInteger -> Input.Lexer s m ()
        goComponent s0 = NumberLexing.tlexScan NumberRules.Component >>= \case
            Tlex.TlexEndOfInput -> do
                Input.yieldTlexError
            Tlex.TlexNotAccepted -> do
                Input.yieldTlexError
            Tlex.TlexAccepted pos1 act -> do
                Input.seekToPosition pos1
                let s1 = case act of
                        NumberRules.LexedComponentElement e -> do
                            let base = lexItemStateOfIntegerBase s0
                                fraction0 = lexItemStateOfIntegerFraction s0
                            s0
                                {
                                    lexItemStateOfIntegerFraction = fraction0 * base + e
                                }
                        NumberRules.LexedSep -> s0
                        NumberRules.LexedDot -> unexpectedAction
                        NumberRules.LexedSignPositive -> unexpectedAction
                        NumberRules.LexedSignNegative -> unexpectedAction
                        NumberRules.LexedBase{} -> unexpectedAction
                if
                    | pos1 < posEnd -> goComponent s1
                    | otherwise -> yieldLitIntegerToken s1

        yieldLitIntegerToken :: LexItemStateOfInteger -> Input.Lexer s m ()
        yieldLitIntegerToken s = do
            let fraction0 = case lexItemStateOfIntegerSign s of
                    True -> lexItemStateOfIntegerFraction s
                    False -> negate do lexItemStateOfIntegerFraction s
                tok = Input.LexedToken do
                    Token.TokLexeme do
                        Token.LitInteger fraction0
            span0 <- Input.lexSpan posStart posEnd
            Input.lexerYield do
                Spanned.Spanned
                    {
                        Spanned.getSpan = span0,
                        Spanned.unSpanned = tok
                    }

lexAndYieldLitString :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldLitString = undefined

lexAndYieldInterpStringStart :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldInterpStringStart = undefined

lexAndYieldInterpStringContinue :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldInterpStringContinue = undefined

lexAndYieldInterpStringEnd :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldInterpStringEnd = undefined

lexAndYieldCommentLineWithContent :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldCommentLineWithContent = undefined

lexAndYieldCommentMultilineWithContent :: MonadST.T s m => Int -> Int -> Input.Lexer s m ()
lexAndYieldCommentMultilineWithContent = undefined
