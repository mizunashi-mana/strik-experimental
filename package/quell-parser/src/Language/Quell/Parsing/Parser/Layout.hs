module Language.Quell.Parsing.Parser.Layout where

import           Language.Quell.Prelude

import qualified Conduit                            as Conduit
import qualified Language.Quell.Frontend.Data.Token as Token
import qualified Language.Quell.Parsing.Spanned     as Spanned


data TokenWithL
    = Token (Spanned.T Token.LexToken)
    | OpenNewImpLayout Position
    | OpenNewExpLayout
    | Newline Position
    | CloseLayout
    deriving (Eq, Show)

data Position
    = PositionEos
    | PositionByCol Int
    deriving (Eq, Show)

instance Ord Position where
    compare p1 p2 = case p1 of
        PositionEos -> case p2 of
            PositionEos ->
                EQ
            PositionByCol{} ->
                LT
        PositionByCol i1 -> case p2 of
            PositionEos ->
                GT
            PositionByCol i2 ->
                compare i1 i2


type PreParseConduit = Conduit.ConduitT (Spanned.T Token.LexToken) TokenWithL

preParse :: forall m. Monad m => Int -> PreParseConduit m ()
preParse = openNewImpLayout where
    openNewImpLayout :: Int -> PreParseConduit m ()
    openNewImpLayout = \currentLineNum -> Conduit.await >>= \case
        Nothing -> do
            Conduit.yield do ExpectNewImpLayout PositionEos
            undefined
        Just spannedTok -> do
            let newLoc = Spanned.beginLoc do Spanned.getSpan spannedTok
            let newCol = Spanned.locCol newLoc
            Conduit.yield do ExpectNewImpLayout do PositionByCol newCol
            undefined
        where
            checkNewline currentLineNum = Conduit.await >>= \case
                Nothing -> pure ()

data TokenType
    = TokenImpLayoutOpen
    | TokenExpLayoutOpen
    | TokenLayoutClose

tokenType :: Token.LexToken -> Maybe TokenType
tokenType = \case
    Token.SpBraceOpen  -> Just TokenImpLayoutOpen
    Token.SpBraceClose -> Just TokenLayoutClose
    Token.SpBrackOpen  -> Just TokenImpLayoutOpen
    Token.SpBrackClose -> Just TokenLayoutClose
    Token.SpParenOpen  -> Just TokenImpLayoutOpen
    Token.SpParenClose -> Just TokenLayoutClose
    Token.KwBraceOpen  -> Just TokenExpLayoutOpen
    Token.KwBrackOpen  -> Just TokenExpLayoutOpen
    Token.KwParenOpen  -> Just TokenExpLayoutOpen
    _                  -> Nothing


type WithLConduit = Conduit.ConduitT TokenWithL (Spanned.T Token.LexToken)

withL :: forall m. Monad m => WithLConduit m ()
withL = go [] where
    go :: [Maybe Position] -> WithLConduit m ()
    go ms = Conduit.await >>= \case
        Nothing ->
            pure ()
        Just tokL -> case tokL of
            OpenNewImpLayout pos -> do
                go do Just pos:ms
            OpenNewExpLayout ->
                go do Nothing:ms
            CloseLayout ->
                undefined
            Newline pos ->
                undefined
            Token spannedTok ->
                undefined
