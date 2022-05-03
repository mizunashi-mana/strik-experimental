module Language.Quell.Prelude.Core (
    module Prelude,

    module Control.Applicative,
    module Control.Monad,
    module Control.Monad.IO.Class,
    module Control.Monad.ST,
    module Control.Monad.Trans.Class,
    module Control.Monad.Trans.Except,
    module Control.Monad.Trans.Reader,
    module Control.Monad.Trans.State.Strict,
    module Data.ByteString,
    module Data.Coerce,
    module Data.Constraint,
    module Data.Function,
    module Data.Functor,
    module Data.Functor.Identity,
    module Data.Functor.Compose,
    module Data.Ix,
    module Data.Kind,
    module Data.List.NonEmpty,
    module Data.MonoTraversable,
    module Data.Ord,
    module Data.Primitive.Types,
    module Data.Proxy,
    module Data.Ratio,
    module Data.Semigroup,
    module Data.Sequences,
    module Data.Text,
    module Data.Typeable,
    module Data.Vector,
    module Data.Word,
    module GHC.Prim,
    module GHC.Stack,
    module GHC.Types,
    module GHC.TypeLits,
    module Prettyprinter,
) where

-- FIXME: hiding (error)
import           Prelude                          hiding (String, break, drop,
                                                   dropWhile, filter, foldl,
                                                   foldr, head, lex, lines, pi,
                                                   replicate, reverse, span,
                                                   splitAt, tail, take,
                                                   takeWhile, unlines, unwords,
                                                   words, ($))

import           Control.Applicative
import           Control.Monad                    hiding (filterM, replicateM)
import           Control.Monad.IO.Class
import           Control.Monad.ST
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       hiding (liftCallCC,
                                                   liftListen, liftPass)
import           Control.Monad.Trans.Reader       hiding (liftCallCC, liftCatch)
import           Control.Monad.Trans.State.Strict hiding (liftCallCC, liftCatch,
                                                   liftListen, liftPass, modify)
import           Data.ByteString                  (ByteString)
import           Data.Coerce
import           Data.Constraint                  (Dict (..), withDict)
import           Data.Function                    hiding (($))
import           Data.Functor
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Ix                          (Ix)
import           Data.Kind                        (Type)
import           Data.List.NonEmpty               (NonEmpty (..))
import           Data.MonoTraversable             hiding (oforM, omapM)
import           Data.Ord                         (Down (..))
import           Data.Primitive.Types             (sizeOf#)
import           Data.Proxy                       (Proxy (..))
import           Data.Ratio
import           Data.Semigroup                   (Semigroup (..))
import           Data.Sequences
import           Data.Text                        (Text)
import           Data.Typeable                    (Typeable)
import           Data.Vector                      (Vector)
import           Data.Word                        (Word, Word8)
import           GHC.Prim
import           GHC.Stack                        (CallStack, HasCallStack,
                                                   callStack)
import           GHC.TypeLits                     (KnownSymbol, Symbol)
import           GHC.Types
import           Prettyprinter                    (Doc, Pretty (..))
