module Language.Quell.Parsing.TestLexer where

import           Language.Quell.Prelude

import qualified Language.Quell.Data.Bag               as Bag
import qualified Language.Quell.Parsing.Lexer.Error    as Error
import qualified Language.Quell.Parsing.Spanned        as Spanned
import qualified Language.Quell.Data.Monad.MonadST     as MonadST
import qualified Language.Quell.Parsing.Lexer          as Lexer


data LexerReport
    = ReportDecodeError Spanned.BytesSpan Text
    | ReportLexError Spanned.Span Error.T Text
    deriving (Eq, Show)

newtype TestLexer s a = TestLexer
    {
        unTestLexer :: ST s (Bag.T LexerReport, a)
    }
    deriving Functor

runTestLexer :: (forall s. TestLexer s a) -> (Bag.T LexerReport, a)
runTestLexer l = runST do unTestLexer l

instance Applicative (TestLexer s) where
    pure x = TestLexer do pure do pure x
    mf <*> mx = TestLexer do
        (rs1, f) <- unTestLexer mf
        (rs2, x) <- unTestLexer mx
        pure (rs1 <> rs2, f x)

instance Monad (TestLexer s) where
    mx >>= f = TestLexer do
        (rs1, x) <- unTestLexer mx
        (rs2, fx) <- unTestLexer do f x
        pure (rs1 <> rs2, fx)

instance MonadST.MonadST s (TestLexer s) where
    type Marker (TestLexer s) = s
    liftST mx = TestLexer do mx <&> \x -> (mempty, x)

instance Lexer.LexerMonad s (TestLexer s) where
    reportDecodeError bsp txt = TestLexer do
        pure (pure do ReportDecodeError bsp txt, ())
    reportLexError sp err txt = TestLexer do
        pure (pure do ReportLexError sp err txt, ())
