module Language.Quell.Parsing.Parser.Layout where

import           Language.Quell.Prelude

import qualified Conduit                            as Conduit
import qualified Language.Quell.Frontend.Data.Token as Token
import qualified Language.Quell.Parsing.Spanned     as Spanned


data TokenWithL
    = TokenRaw (Spanned.T Token.LexToken)
    | TokenVirt VirtToken
    | LayoutError
    deriving (Eq, Show)

data VirtToken
    = VirtSemi
    deriving (Eq, Show)

layoutProcess :: Monad m
    => Bool -> Int -> Conduit.ConduitT (Spanned.T Token.LexToken) TokenWithL m ()
layoutProcess expOpenLayout currentLineNum =
    preParse expOpenLayout currentLineNum Conduit..| withL


data TokenPreParsed
    = TokenThrough (Spanned.T Token.LexToken)
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


type PreParseConduit = Conduit.ConduitT (Spanned.T Token.LexToken) TokenPreParsed

preParse :: forall m. Monad m => Bool -> Int -> PreParseConduit m ()
preParse expOpenLayout = \currentLineNum -> if
        | expOpenLayout ->
            openNewImpLayout currentLineNum
        | otherwise ->
            checkNewline currentLineNum
    where
        checkNewline currentLineNum = Conduit.await >>= \case
            Nothing -> if
                | expOpenLayout ->
                    Conduit.yield CloseLayout
                | otherwise ->
                    pure ()
            Just spannedTok -> do
                let loc = Spanned.beginLoc do Spanned.getSpan spannedTok
                let newLineNum = Spanned.locLine loc
                if
                    | currentLineNum < newLineNum -> do
                        Conduit.yield do Newline do PositionByCol do Spanned.locCol loc
                        checkTokenType spannedTok newLineNum
                    | otherwise -> do
                        checkTokenType spannedTok currentLineNum

        checkTokenType spannedTok currentLineNum =
            case tokenType do Spanned.unSpanned spannedTok of
                Just TokenImpLayoutOpen -> do
                    Conduit.yield do TokenThrough spannedTok
                    openNewImpLayout currentLineNum
                Just TokenExpLayoutOpen -> do
                    Conduit.yield do TokenThrough spannedTok
                    Conduit.yield OpenNewExpLayout
                    checkNewline currentLineNum
                Just TokenLayoutClose -> do
                    Conduit.yield do TokenThrough spannedTok
                    Conduit.yield CloseLayout
                    checkNewline currentLineNum
                Nothing -> do
                    Conduit.yield do TokenThrough spannedTok
                    checkNewline currentLineNum

        openNewImpLayout currentLineNum = Conduit.await >>= \case
            Nothing -> do
                Conduit.yield do OpenNewImpLayout PositionEos
                checkNewline currentLineNum
            Just spannedTok -> do
                let loc = Spanned.beginLoc do Spanned.getSpan spannedTok
                let colNum = Spanned.locCol loc
                Conduit.yield do OpenNewImpLayout do PositionByCol colNum
                checkNewline currentLineNum


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


type WithLConduit = Conduit.ConduitT TokenPreParsed TokenWithL

withL :: forall m. Monad m => WithLConduit m ()
withL = go [] where
    go :: [Maybe Position] -> WithLConduit m ()
    go ms0 = Conduit.await >>= \case
        Nothing ->
            pure ()
        Just tokL -> case tokL of
            OpenNewImpLayout pos -> do
                go do Just pos:ms0
            OpenNewExpLayout ->
                go do Nothing:ms0
            CloseLayout -> case ms0 of
                [] ->
                    Conduit.yield LayoutError
                _:ms1 ->
                    go ms1
            Newline pos -> case ms0 of
                [] ->
                    Conduit.yield LayoutError
                Just posL:_ -> if
                    | pos < posL ->
                        Conduit.yield LayoutError
                    | pos == posL -> do
                        Conduit.yield do TokenVirt VirtSemi
                        go ms0
                    | otherwise ->
                        go ms0
                Nothing:_ ->
                    go ms0
            TokenThrough spannedTok -> do
                Conduit.yield do TokenRaw spannedTok
                go ms0
