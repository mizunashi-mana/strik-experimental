module Language.Quell.Parsing.Parser.Layout (
    T,

    TokenWithL (..),
    preParseForProgram,
    preParse,

    LayoutPos (..),

    isLayoutKeyword,
    isExplicitOpenBrace,
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Language.Quell.Parsing.Spanned as Spanned
import qualified Language.Quell.Type.Token      as Token


type T = TokenWithL

data TokenWithL
    = Token (Spanned.T Token.LexToken)
    | ExpectNewImplicitLayout LayoutPos
    | Newline LayoutPos
    deriving (Eq, Show)

data LayoutPos
    = LayoutPosByCol Int
    | LayoutPosEos
    deriving (Eq, Show)

type WithLConduit = Conduit.ConduitT (Spanned.T Token.LexToken) TokenWithL

preParseForProgram :: Monad m => WithLConduit m ()
preParseForProgram = preParse True do Spanned.locLine Spanned.initialLoc

preParse :: Monad m => Bool -> Int -> WithLConduit m ()
preParse = go1 where
    go1 expBrace l0 = Conduit.await >>= \case
        Nothing
            | expBrace -> do
                Conduit.yield do ExpectNewImplicitLayout LayoutPosEos
                Conduit.yield do Newline LayoutPosEos
                pure ()
            | otherwise ->
                pure ()
        Just tok -> do
            let loc1 = Spanned.beginLoc do Spanned.getSpan tok
            if
                | isExplicitOpenBrace do Spanned.unSpanned tok -> do
                    go2 tok
                | expBrace -> do
                    let lpos = LayoutPosByCol do Spanned.locCol loc1
                    Conduit.yield do ExpectNewImplicitLayout lpos
                    Conduit.yield do Newline lpos
                    go2 tok
                | l0 < Spanned.locLine loc1 -> do
                    let lpos = LayoutPosByCol do Spanned.locCol loc1
                    Conduit.yield do Newline lpos
                    go2 tok
                | otherwise -> do
                    go2 tok

    go2 tok = do
        Conduit.yield do Token tok
        go1
            do isLayoutKeyword do Spanned.unSpanned tok
            do Spanned.locLine do Spanned.endLoc do Spanned.getSpan tok

isLayoutKeyword :: Token.LexToken -> Bool
isLayoutKeyword = \case
    Token.KwCase      -> True
    Token.KwLet       -> True
    Token.KwLetrec    -> True
    Token.KwWith      -> True
    Token.KwWhen      -> True
    Token.KwWhere     -> True
    Token.SpBlock     -> True
    Token.SpTypeBlock -> True
    _                 -> False

isExplicitOpenBrace :: Token.LexToken -> Bool
isExplicitOpenBrace = \case
    Token.SpBraceOpen            -> True
    Token.SpDBraceOpen           -> True
    _                            -> False
