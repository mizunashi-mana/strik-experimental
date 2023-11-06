module Language.Quell.Parsing.Lexer.Input where

import           Language.Quell.Prelude

import qualified Conduit                               as Conduit
import qualified Language.Lexer.Tlex                   as Tlex
import qualified Language.Quell.Data.BufferedConduit   as BufferedConduit
import qualified Language.Quell.Data.Monad.MonadST     as MonadST
import qualified Language.Quell.Frontend.Data.Token    as Token
import qualified Language.Quell.Model.Error            as Error
import qualified Language.Quell.Model.ErrorCode        as ErrorCode
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Quell.Parsing.Spanned        as Spanned

data LexedUnit
    = LexedToken Token.T
    | LexError Error.T
    deriving Show

type LexerInput = (Spanned.BytesSpan, Char)
type LexerInputUnit = Spanned.T (Char, CodeUnit.T)
type LexerOutput = Spanned.T LexedUnit

type BuildInputUnit = Maybe LexerInputUnit -> LexerInput -> LexerInputUnit

newtype Lexer s m a = Lexer
    { unLexer :: BufferedConduit.T s LexerInputUnit LexerOutput m a
    }
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadIO
    ) via BufferedConduit.T s LexerInputUnit LexerOutput m

runLexer :: MonadST.T s m
    => BuildInputUnit -> Lexer s m () -> Conduit.ConduitT LexerInput LexerOutput m ()
runLexer buildInputUnit lexer = buildInputUnits buildInputUnit
    Conduit..| BufferedConduit.runConduitT do unLexer lexer

buildInputUnits :: Monad m
    => BuildInputUnit -> Conduit.ConduitT LexerInput LexerInputUnit m ()
buildInputUnits buildInputUnit = go Nothing where
    go mLastUnit = Conduit.await >>= \case
        Just i -> do
            let newUnit = buildInputUnit mLastUnit i
            Conduit.yield newUnit
            go do Just newUnit
        Nothing ->
            pure ()

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

seekAndSetNeedBackModeTo :: MonadST.T s m => Int -> Lexer s m ()
seekAndSetNeedBackModeTo p = Lexer do BufferedConduit.seekAndSetNeedBackModeTo p

foldAwaits :: MonadST.T s m => LexItemState a -> Int -> Lexer s m a
foldAwaits s0 c0 = if
    | c0 <= 0 ->
        pure do lexItemState s0
    | otherwise ->
        lexerAwait >>= \case
            Nothing ->
                pure do lexItemState s0
            Just x -> do
                let s1 = lexItemNextState s0 x
                foldAwaits s1 do c0 - 1

consumeLexedUnits :: MonadST.T s m => LexItemState a -> Int -> Int -> Lexer s m a
consumeLexedUnits s0 pos0 pos1 = do
    seekToPosition pos0
    foldAwaits s0 do pos1 - pos0

consumeLexedUnitsAndSwitchToNoBackMode :: MonadST.T s m
    => LexItemState a -> Int -> Int -> Lexer s m a
consumeLexedUnitsAndSwitchToNoBackMode s0 pos0 pos1 = do
    seekToPosition pos0
    setNoBackMode
    foldAwaits s0 do pos1 - pos0

lexSpan :: MonadST.T s m => Int -> Int -> Lexer s m (Maybe Spanned.Span)
lexSpan pos0 pos1 = consumeLexedUnitsAndSwitchToNoBackMode
    do LexItemState
        {
            lexItemState = Nothing,
            lexItemNext = go
        }
    pos0 pos1
    where
        go mspan0 spannedItem = do
            let span1 = case mspan0 of
                    Nothing    -> Spanned.getSpan spannedItem
                    Just span0 -> span0 <> Spanned.getSpan spannedItem
            LexItemState
                {
                    lexItemState = Just span1,
                    lexItemNext = go
                }

-- FIXME: try error recovering and report detailed and suggestions
yieldTlexError :: MonadST.T s m => Lexer s m ()
yieldTlexError = lexerYield do
    Spanned.Spanned
        { Spanned.getSpan = error "TODO: yieldTlexError"
        , Spanned.unSpanned = LexError do
            Error.build ErrorCode.UnexpectedCodeUnits "TODO"
        }


data LexItemState a = LexItemState
    {
        lexItemState :: a,
        lexItemNext  :: LexItemStateNext a
    }

type LexItemStateNext a = a -> LexerInputUnit -> LexItemState a

lexItemNextState :: LexItemState a -> LexerInputUnit -> LexItemState a
lexItemNextState s = lexItemNext s do lexItemState s

skipLexItems :: Int -> LexItemStateNext a -> LexItemStateNext a
skipLexItems skipCount cont s0 item = if
    | skipCount <= 0 -> cont s0 item
    | otherwise -> LexItemState
        {
            lexItemState = s0,
            lexItemNext = skipLexItems
                do skipCount - 1
                do cont
        }
