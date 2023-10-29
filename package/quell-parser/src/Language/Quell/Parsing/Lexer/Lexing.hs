{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Lexing where

import Language.Quell.Prelude

import qualified Language.Quell.Parsing.Lexer.Rules as Rules
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Frontend.Data.Token as Token
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Quell.Model.Error as Error
import qualified Language.Quell.Data.Monad.MonadST as MonadST
import qualified Language.Quell.Data.BufferedConduit as BufferedConduit
import qualified Language.Lexer.Tlex as Tlex
import qualified Language.Quell.Model.ErrorCode as ErrorCode
import qualified Conduit as Conduit
import qualified Data.EnumSet as EnumSet

$(Rules.buildLexer)

data LexedUnit
    = LexedToken Token.T
    | LexError Error.T
    deriving Show

type LexerInput = (Spanned.BytesSpan, Char)
type LexerInputUnit = Spanned.T (Char, CodeUnit.T)
type LexerOutput = Spanned.T LexedUnit

runLexer :: MonadST.T s m => Lexer s m () -> Conduit.ConduitT LexerInput LexerOutput m ()
runLexer m = buildInputUnits
    Conduit..| BufferedConduit.runConduitT do unLexer m

buildInputUnits :: Monad m => Conduit.ConduitT LexerInput LexerInputUnit m ()
buildInputUnits = go Nothing where
    go mLastUnit = Conduit.await >>= \case
        Just i -> do
            let newUnit = buildInputUnit mLastUnit i
            Conduit.yield newUnit
            go do Just newUnit
        Nothing ->
            pure ()

buildInputUnit :: Maybe LexerInputUnit -> LexerInput -> LexerInputUnit
buildInputUnit mLastItem (bs, c) = Spanned.Spanned
    { Spanned.unSpanned = (c, u)
    , Spanned.getSpan = sp
    }
    where
        u = CodeUnit.fromChar c

        newlineCRLF = [CodeUnit.CodeUnitByPoint CodeUnit.LcU000D, CodeUnit.CodeUnitByPoint CodeUnit.LcU000A]
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
                lexAndYield pos0 pos1 act
                go Rules.Initial

    lexAndYield pos0 pos1 = \case
        Rules.WithWhiteSpace -> do
            -- The next mode change clean the buffer.
            seekToPosition pos1
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

newtype Lexer s m a = Lexer
    { unLexer :: BufferedConduit.T s LexerInputUnit LexerOutput m a
    }
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadIO
    ) via BufferedConduit.T s LexerInputUnit LexerOutput m

instance MonadST.T s m => MonadST.MonadST s (Lexer s m) where
    type Marker (Lexer s m) = MonadST.Marker m
    liftST mx = Lexer do MonadST.liftST mx

instance MonadST.T s m => Tlex.TlexContext Int CodeUnit.T (Lexer s m) where
    tlexGetInputPart = lexerAwait >>= \case
        Nothing ->
            pure Nothing
        Just iu -> do
            let (_, u) = Spanned.unSpanned iu
            pure do Just u

    tlexGetMark = getCurrentPosition

lexerAwait :: MonadST.T s m => Lexer s m (Maybe LexerInputUnit)
lexerAwait = Lexer BufferedConduit.await

lexerYield :: Monad m => LexerOutput -> Lexer s m ()
lexerYield u = Lexer do BufferedConduit.yield u

getCurrentPosition :: Monad m => Lexer s m Int
getCurrentPosition = Lexer BufferedConduit.getCurrentPosition

seekToPosition :: MonadST.T s m => Int -> Lexer s m ()
seekToPosition p = Lexer do BufferedConduit.seekToPosition p

setNeedBackMode :: MonadST.T s m => Int -> Lexer s m ()
setNeedBackMode p = Lexer do BufferedConduit.setBufferMode do BufferedConduit.NeedBack p

setNoBackMode :: MonadST.T s m => Lexer s m ()
setNoBackMode = Lexer do BufferedConduit.setBufferMode BufferedConduit.NoBack

foldAwaits1 :: MonadST.T s m
    => (LexerInputUnit -> a) -> (a -> LexerInputUnit -> a)
    -> Int -> Lexer s m (Maybe a)
foldAwaits1 f0 f = \c0 -> if
    | c0 <= 0 ->
        pure Nothing
    | otherwise ->
        lexerAwait >>= \case
            Nothing ->
                pure Nothing
            Just x ->
                go
                    do c0 - 1
                    do f0 x
    where
        go c0 z0 = if
            | c0 <= 0 ->
                pure do Just z0
            | otherwise ->
                lexerAwait >>= \case
                    Nothing ->
                        pure do Just z0
                    Just x ->
                        go
                            do c0 - 1
                            do f z0 x

consumeLexedUnitsAndSwitchToNoBackMode :: MonadST.T s m
    => (LexerInputUnit -> a) -> (a -> LexerInputUnit -> a)
    -> Int -> Int -> Lexer s m a
consumeLexedUnitsAndSwitchToNoBackMode f0 f pos0 pos1 = do
    seekToPosition pos0
    setNoBackMode
    mx <- foldAwaits1 f0 f
        do pos1 - pos0
    case mx of
        Nothing ->
            error "unreachable: lexer should consume some inputs."
        Just x ->
            pure x

lexSpan :: MonadST.T s m => Int -> Int -> Lexer s m Spanned.Span
lexSpan pos0 pos1 = consumeLexedUnitsAndSwitchToNoBackMode
    do \item -> Spanned.getSpan item
    do \sp item -> sp <> Spanned.getSpan item
    pos0 pos1

-- FIXME: try error recovering and report detailed and suggestions
yieldTlexError :: MonadST.T s m => Lexer s m ()
yieldTlexError = lexerYield do
    Spanned.Spanned
        { Spanned.getSpan = error "todo: yieldTlexError"
        , Spanned.unSpanned = LexError do
            Error.build ErrorCode.UnexpectedCodeUnits "TODO"
        }

yieldToken :: MonadST.T s m => Int -> Int -> Token.T -> Lexer s m ()
yieldToken pos0 pos1 t = do
    sp0 <- lexSpan pos0 pos1
    let u = Spanned.Spanned
            { Spanned.getSpan = sp0
            , Spanned.unSpanned = LexedToken t
            }
    lexerYield u

yieldIdType :: MonadST.T s m => Int -> Int -> Rules.IdType -> Lexer s m ()
yieldIdType = undefined

yieldKwToken :: MonadST.T s m => Int -> Int -> Lexer s m ()
yieldKwToken = undefined

lexAndYieldFreeId :: MonadST.T s m => Int -> Int -> Lexer s m ()
lexAndYieldFreeId = undefined

lexAndYieldLitRationalWithDot :: MonadST.T s m => Int -> Int -> Lexer s m ()
lexAndYieldLitRationalWithDot = undefined

lexAndYieldLitDefaultInteger :: MonadST.T s m => Int -> Int -> Lexer s m ()
lexAndYieldLitDefaultInteger = undefined

lexAndYieldLitDecimalInteger :: MonadST.T s m => Int -> Int -> Lexer s m ()
lexAndYieldLitDecimalInteger = undefined

lexAndYieldLitHeximalInteger :: MonadST.T s m => Int -> Int -> Lexer s m ()
lexAndYieldLitHeximalInteger = undefined

lexAndYieldInterpStringStart :: MonadST.T s m => Int -> Int -> Lexer s m ()
lexAndYieldInterpStringStart = undefined

lexAndYieldInterpStringContinue :: MonadST.T s m => Int -> Int -> Lexer s m ()
lexAndYieldInterpStringContinue = undefined

lexAndYieldCommentLineWithContent :: MonadST.T s m => Int -> Int -> Lexer s m ()
lexAndYieldCommentLineWithContent = undefined

lexAndYieldCommentMultilineWithContent :: MonadST.T s m => Int -> Int -> Lexer s m ()
lexAndYieldCommentMultilineWithContent = undefined
