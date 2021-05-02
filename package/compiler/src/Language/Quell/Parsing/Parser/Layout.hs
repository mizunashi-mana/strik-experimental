module Language.Quell.Parsing.Parser.Layout (
    T,
    Layout (..),

    TokenWithL (..),
    preParseForProgram,
    preParse,

    isLayoutKeyword,

    isOpen,
    isClose,
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Type.Token      as Token


type T = Layout

data Layout
    = NoLayout Token.T
    | ExplicitBrace
    | ExplicitDBrace Int
    | VirtualBrace Int
    deriving (Eq, Show)

data TokenWithL
    = Token Bool (Spanned.T Token.T)
    | ExpectBrace
    deriving (Eq, Show)

type WithLConduit = Conduit.ConduitT (Spanned.T Token.T) TokenWithL

preParseForProgram :: Monad m => WithLConduit m ()
preParseForProgram = do
    Conduit.yield ExpectBrace
    preParse

preParse :: Monad m => WithLConduit m ()
preParse = go 0 where
    go pl = resolveNewline pl \isN spt l -> case Spanned.unSpanned spt of
        t | isLayoutKeyword t -> do
            Conduit.yield do Token isN spt
            Conduit.yield ExpectBrace
            go l
        _ -> do
            Conduit.yield do Token isN spt
            go l

resolveNewline :: Monad m
    => Int
    -> (Bool -> Spanned.T Token.T -> Int -> WithLConduit m ())
    -> WithLConduit m ()
resolveNewline pl cont = Conduit.await >>= \case
    Nothing ->
        pure ()
    Just spt -> do
        let sp = Spanned.getSpan spt
            l1 = Spanned.locLine do Spanned.beginLoc sp
            l2 = Spanned.locLine do Spanned.endLoc sp
        if
            | pl < l1 -> do
                cont True spt l2
            | otherwise ->
                cont False spt l2

isLayoutKeyword :: Token.T -> Bool
isLayoutKeyword = \case
    Token.KwCase      -> True
    Token.KwLet       -> True
    Token.KwLetrec    -> True
    Token.KwWith      -> True
    Token.KwWhen      -> True
    Token.KwWhere     -> True
    Token.SymLambda   -> True
    Token.SpBlock     -> True
    Token.SpTypeBlock -> True
    _                 -> False

isOpen :: Token.T -> Bool
isOpen = \case
    Token.SpParenOpen            -> True
    Token.SpBrackOpen            -> True
    Token.SpBraceOpen            -> True
    Token.SpDBraceOpen           -> True
    Token.LitInterpStringStart{} -> True
    _                            -> False

isClose :: Token.T -> Bool
isClose = \case
    Token.SpParenClose         -> True
    Token.SpBrackClose         -> True
    Token.SpBraceClose         -> True
    Token.SpDBraceClose        -> True
    Token.LitInterpStringEnd{} -> True
    _                          -> False
