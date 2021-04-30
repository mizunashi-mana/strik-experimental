module Language.Quell.Parsing.Parser.Runner (
    T,
    Runner,
    RunnerResult (..),
    RunnerContext (..),
    runRunner,
    lexer,
    reportParseError,
    parseError
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Data.Bag              as Bag
import qualified Language.Quell.Parsing.Parser.Error  as Error
import qualified Language.Quell.Parsing.Parser.Layout as Layout
import qualified Language.Quell.Parsing.Spanned       as Spanned
import qualified Language.Quell.Type.Token            as Token


type T = Runner

type RunnerConduitT = Conduit.ConduitT
    Layout.TokenWithL
    Conduit.Void

newtype Runner m a = Runner
    (
        RunnerConduitT
            (ExceptT () (StateT (RunnerContext m) m))
            a
    )
    deriving (
        Functor,
        Applicative,
        Monad
    ) via RunnerConduitT
        (ExceptT () (StateT (RunnerContext m) m))

runRunner :: Monad m => Runner m a -> RunnerConduitT m (RunnerResult a)
runRunner (Runner m) = do
    (mx, ctx) <- Conduit.runStateC initialContext do Conduit.runExceptC m
    let errs = otoList do parseErrors ctx
    pure case mx of
        Right x | errs == [] ->
            RunnerSuccess x
        _ ->
            RunnerFailed errs

data RunnerResult a
    = RunnerSuccess a
    | RunnerFailed [Spanned.T Error.T]
    deriving (Eq, Show, Functor)

type RunnerCont m a = Spanned.T Token.T -> Runner m a

data RunnerContext m = RunnerContext
    {
        withLCont   :: forall a. RunnerCont m a -> Runner m a,
        lastSpan    :: Spanned.Span,
        tokenStack  :: [Layout.TokenWithL],
        parseErrors :: Bag.T (Spanned.T Error.T)
    }

initialContext :: Monad m => RunnerContext m
initialContext = RunnerContext
    {
        withLCont = \p -> withL p False [],
        lastSpan = Spanned.Span
            {
                Spanned.beginLoc = lastLoc,
                Spanned.endLoc = lastLoc
            },
        tokenStack = [],
        parseErrors = mempty
    }
    where
        lastLoc = Spanned.Loc
            {
                Spanned.locLine = 0,
                Spanned.locCol = 0,
                Spanned.locBytesPos = 0
            }

lexer :: Monad m => RunnerCont m a -> Runner m a
lexer p0 = do
    ctx0 <- runnerGet
    withLCont ctx0 \spt -> debugTrace ("parsing: " <> show spt) do p0 spt

withL :: Monad m => RunnerCont m a -> Bool -> [Layout.T] -> Runner m a
withL p0 expB ms = consumeToken >>= \case
    Nothing -> do
        ctx0 <- runnerGet
        let lc = Spanned.endLoc do lastSpan ctx0
        resolveEmptyBrace p0 expB lc \p1 ->
            tryEnd p1 ms
    Just twl -> case twl of
        Layout.Token isN spt
            | isN ->
                resolveNewline p0 spt expB ms
            | otherwise ->
                resolveToken p0 spt expB ms
        Layout.ExpectBrace ->
            withL p0 True ms

reportParseError :: Monad m => RunnerCont m a -> Error.T -> Spanned.Span -> Runner m a
reportParseError p0 err sp = do
    let spe = Spanned.Spanned
            {
                Spanned.getSpan = sp,
                Spanned.unSpanned = err
            }
    ctx0 <- runnerGet
    runnerPut do
        ctx0
            {
                parseErrors = snoc (parseErrors ctx0) spe
            }
    let spt = Spanned.spannedFromLoc
            do Spanned.endLoc do lastSpan ctx0
            do Token.EndOfSource
    p0 spt -- FIXME: Try to recover error

-- FIXME: Try to analysis error and recover
parseError :: Monad m => Runner m a
parseError = do
    ctx <- runnerGet
    debugTraceShow (lastSpan ctx, tokenStack ctx)
        do Runner do lift do throwE ()

resolveToken :: Monad m => RunnerCont m a
    -> Spanned.T Token.T -> Bool -> [Layout.T] -> Runner m a
resolveToken p0 spt expB ms = do
    case Spanned.unSpanned spt of
        Token.SpBraceOpen | expB ->
            runParserL p0 spt \p1 ->
                withL p1 False do Layout.ExplicitBrace:ms
        Token.SpDBraceOpen | expB ->
            runParserL p0 spt \p1 -> do
                m <- calcLayoutPos
                withL p1 False do Layout.ExplicitDBrace m:ms
        _ | expB -> do
            let vbOp = Spanned.spannedFromLoc
                    do Spanned.beginLoc do Spanned.getSpan spt
                    do Token.SpVBraceOpen
            runParserL p0 vbOp \p1 -> do
                m <- calcLayoutPos
                resolveToken p1 spt False do Layout.VirtualBrace m:ms
        t | Layout.isOpen t ->
            runParserL p0 spt \p1 -> do
                withL p1 False do Layout.NoLayout t:ms
        t | Layout.isClose t ->
            tryClose p0 spt ms
        Token.LitInterpStringContinue{} ->
            tryClose p0 spt ms
        _ ->
            runParserL p0 spt \p1 -> do
                withL p1 False ms

resolveNewline :: Monad m
    => RunnerCont m a -> Spanned.T Token.T -> Bool -> [Layout.T] -> Runner m a
resolveNewline p0 spt expB ms0 = do
    let bl = Spanned.beginLoc do Spanned.getSpan spt
        c = Spanned.locCol bl
    case ms0 of
        Layout.ExplicitDBrace m:ms1
            | c < m -> case Spanned.unSpanned spt of
                Token.SpDBraceClose ->
                    resolveEmptyBrace p0 expB bl \p1 ->
                        runParserL p1 spt \p2 ->
                            withL p2 False ms1
                _ -> reportParseError p0
                    Error.ExpectedDBraceClose
                    do Spanned.getSpan spt
            | c == m ->
                resolveEmptyBrace p0 expB bl \p1 -> do
                    let vsemi = Spanned.spannedFromLoc bl
                            Token.SpVSemi
                    runParserL p1 vsemi \p2 ->
                        resolveToken p2 spt False ms0
            | otherwise ->
                resolveToken p0 spt False ms0
        Layout.VirtualBrace m:ms1
            | c < m ->
                resolveEmptyBrace p0 expB bl \p1 -> do
                    let vbCl = Spanned.spannedFromLoc bl
                            Token.SpVBraceClose
                    runParserL p1 vbCl \p2 ->
                        resolveNewline p2 spt False ms1
            | c == m ->
                resolveEmptyBrace p0 expB bl \p1 -> do
                    let vsemi = Spanned.spannedFromLoc bl
                            Token.SpVSemi
                    runParserL p1 vsemi \p2 ->
                        resolveToken p2 spt False ms0
            | otherwise ->
                resolveToken p0 spt expB ms0
        _ ->
            resolveToken p0 spt expB ms0

resolveEmptyBrace :: Monad m
    => RunnerCont m a -> Bool -> Spanned.Loc
    -> (forall b. RunnerCont m b -> Runner m b) -> Runner m a
resolveEmptyBrace p0 expB lc cont = case expB of
    False ->
        cont p0
    True -> do
        let vbOp = Spanned.spannedFromLoc lc
                Token.SpVBraceOpen
        runParserL p0 vbOp \p1 -> do
            let vbCl = Spanned.spannedFromLoc lc
                    Token.SpVBraceClose
            runParserL p1 vbCl \p2 ->
                cont p2

tryClose :: Monad m
    => RunnerCont m a -> Spanned.T Token.T -> [Layout.T] -> Runner m a
tryClose p0 spt ms0 = case ms0 of
    [] ->
        reportParseErrorNotOpened p0
            do Spanned.getSpan spt
            do Spanned.unSpanned spt
    m:ms1 -> case m of
        Layout.VirtualBrace{} -> do
            let lc = Spanned.beginLoc do Spanned.getSpan spt
                vbCl = Spanned.spannedFromLoc lc
                    Token.SpVBraceClose
            runParserL p0 vbCl \p1 ->
                tryClose p1 spt ms1
        Layout.ExplicitBrace{} -> case Spanned.unSpanned spt of
            Token.SpBraceClose -> runParserL p0 spt \p1 ->
                withL p1 False ms1
            _ -> reportParseError p0
                Error.ExpectedBraceClose
                do Spanned.getSpan spt
        Layout.ExplicitDBrace{} -> case Spanned.unSpanned spt of
            Token.SpDBraceClose -> runParserL p0 spt \p1 ->
                withL p1 False ms1
            _ -> reportParseError p0
                Error.ExpectedDBraceClose
                do Spanned.getSpan spt
        Layout.NoLayout{} -> case Spanned.unSpanned spt of
            t@Token.LitInterpStringContinue{} -> runParserL p0 spt \p1 ->
                withL p1 False do Layout.NoLayout t:ms1
            _ -> runParserL p0 spt \p1 ->
                withL p1 False ms1

tryEnd :: Monad m => RunnerCont m a -> [Layout.T] -> Runner m a
tryEnd = \p0 ms0 -> do
    ctx0 <- runnerGet
    let lc = Spanned.endLoc do lastSpan ctx0
    go lc p0 ms0
    where
        go :: Monad m
            => Spanned.Loc -> RunnerCont m b -> [Layout.T] -> Runner m b
        go lc p0 ms0 = case ms0 of
            [] -> do
                let eos = Spanned.spannedFromLoc lc
                        Token.EndOfSource
                runParserL p0 eos \_ ->
                    error "unreachable: must be ended parsing with EOS."
            m:ms1 -> case m of
                Layout.VirtualBrace _ -> do
                    let vbCl = Spanned.spannedFromLoc lc
                            Token.SpVBraceClose
                    runParserL p0 vbCl \p1 ->
                        go lc p1 ms1
                Layout.ExplicitBrace{} ->
                    reportParseError p0
                        Error.ExpectedBraceClose
                        do Spanned.spanFromLoc lc
                Layout.ExplicitDBrace{} ->
                    reportParseError p0
                        Error.ExpectedDBraceClose
                        do Spanned.spanFromLoc lc
                Layout.NoLayout t ->
                    reportParseErrorExpectedClose p0
                        do Spanned.spanFromLoc lc
                        do t

reportParseErrorNotOpened :: Monad m
    => RunnerCont m a -> Spanned.Span -> Token.T -> Runner m a
reportParseErrorNotOpened p0 sp = \case
    Token.SpParenClose              ->
        reportParseError p0
            Error.NotOpenedParenClose
            sp
    Token.SpBrackClose              ->
        reportParseError p0
            Error.NotOpenedBrackClose
            sp
    Token.SpBraceClose              ->
        reportParseError p0
            Error.NotOpenedBraceClose
            sp
    Token.SpDBraceClose             ->
        reportParseError p0
            Error.NotOpenedDBraceClose
            sp
    Token.LitInterpStringEnd{}      ->
        reportParseError p0
            Error.NotOpenedInterpStringClose
            sp
    Token.LitInterpStringContinue{} ->
        reportParseError p0
            Error.NotOpenedInterpStringClose
            sp
    _ ->
        error "unreachable: the argument token must be an close token."

reportParseErrorExpectedClose :: Monad m
    => RunnerCont m a -> Spanned.Span -> Token.T -> Runner m a
reportParseErrorExpectedClose p0 sp = \case
    Token.SpParenOpen               ->
        reportParseError p0
            Error.ExpectedParenClose
            sp
    Token.SpBrackOpen               ->
        reportParseError p0
            Error.ExpectedBrackClose
            sp
    Token.SpBraceOpen               ->
        reportParseError p0
            Error.ExpectedBraceClose
            sp
    Token.SpDBraceOpen              ->
        reportParseError p0
            Error.ExpectedDBraceClose
            sp
    Token.LitInterpStringStart{}    ->
        reportParseError p0
            Error.ExpectedInterpStringClose
            sp
    Token.LitInterpStringContinue{} ->
        reportParseError p0
            Error.ExpectedInterpStringClose
            sp
    _                               ->
        error "unreachable: the argument token must be an open token."


runParserL :: Monad m
    => RunnerCont m a -> Spanned.T Token.T
    -> (forall b. RunnerCont m b -> Runner m b) -> Runner m a
runParserL p spt cont = do
    runnerModify' \ctx -> ctx
        {
            withLCont = cont
        }
    p spt

calcLayoutPos :: forall m. Monad m => Runner m Int
calcLayoutPos = do
    ctx0 <- runnerGet
    go0 do tokenStack ctx0
    where
        go0 :: [Layout.TokenWithL] -> Runner m Int
        go0 = \case
            Layout.Token _ spt:_ -> do
                let sp = Spanned.getSpan spt
                    c = Spanned.locCol do Spanned.beginLoc sp
                pure c
            Layout.ExpectBrace:ts ->
                go0 ts
            [] ->
                go1 []

        go1 :: [Layout.TokenWithL] -> Runner m Int
        go1 ts0 = Runner Conduit.await >>= \case
            Nothing -> do
                restoreTokenStack ts0
                pure 0
            Just t -> do
                case t of
                    Layout.Token _ spt -> do
                        let sp = Spanned.getSpan spt
                            c = Spanned.locCol do Spanned.beginLoc sp
                        restoreTokenStack do t:ts0
                        pure c
                    Layout.ExpectBrace -> do
                        go1 do t:ts0

        restoreTokenStack ts = runnerModify' \ctx -> ctx
            {
                -- expect token stack having few elements
                tokenStack = tokenStack ctx ++ reverse ts
            }

consumeToken :: Monad m => Runner m (Maybe Layout.TokenWithL)
consumeToken = do
    ctx0 <- runnerGet
    mt <- case tokenStack ctx0 of
        [] ->
            Runner Conduit.await
        t:ts -> do
            runnerPut do
                ctx0
                    {
                        tokenStack = ts
                    }
            pure do Just t
    ctx1 <- runnerGet
    case mt of
        Just (Layout.Token _ spt) -> do
            runnerPut do
                ctx1
                    {
                        lastSpan = Spanned.getSpan spt
                    }
        _ ->
            pure ()
    pure mt

runnerGet :: Monad m => Runner m (RunnerContext m)
runnerGet = Runner do lift do lift get

runnerPut :: Monad m => RunnerContext m -> Runner m ()
runnerPut x = Runner do lift do lift do put x

runnerModify' :: Monad m => (RunnerContext m -> RunnerContext m) -> Runner m ()
runnerModify' f = Runner do lift do lift do modify' f
