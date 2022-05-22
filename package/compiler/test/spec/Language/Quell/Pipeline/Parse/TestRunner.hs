module Language.Quell.Pipeline.Parse.TestRunner where

import           Language.Quell.Prelude

import qualified Language.Quell.Data.Bag            as Bag
import qualified Language.Quell.Data.Monad.MonadST  as MonadST
import qualified Language.Quell.Parsing.Lexer       as Lexer
import qualified Language.Quell.Parsing.Lexer.Error as Error
import qualified Language.Quell.Parsing.Spanned     as Spanned


data Report
    = ReportDecodeError Spanned.BytesSpan Text
    | ReportLexError Spanned.Span Error.T Text
    deriving (Eq, Show)

newtype TestRunner s a = TestRunner
    {
        unTestRunner :: ST s (Bag.T Report, a)
    }
    deriving Functor

runTestRunner :: (forall s. TestRunner s a) -> (Bag.T Report, a)
runTestRunner l = runST do unTestRunner l

instance Applicative (TestRunner s) where
    pure x = TestRunner do pure do pure x
    mf <*> mx = TestRunner do
        (rs1, f) <- unTestRunner mf
        (rs2, x) <- unTestRunner mx
        pure (rs1 <> rs2, f x)

instance Monad (TestRunner s) where
    mx >>= f = TestRunner do
        (rs1, x) <- unTestRunner mx
        (rs2, fx) <- unTestRunner do f x
        pure (rs1 <> rs2, fx)

instance MonadST.MonadST s (TestRunner s) where
    type Marker (TestRunner s) = s
    liftST mx = TestRunner do mx <&> \x -> (mempty, x)

instance Lexer.LexerMonad s (TestRunner s) where
    reportDecodeError bsp txt = TestRunner do
        pure (pure do ReportDecodeError bsp txt, ())
    reportLexError sp err txt = TestRunner do
        pure (pure do ReportLexError sp err txt, ())
