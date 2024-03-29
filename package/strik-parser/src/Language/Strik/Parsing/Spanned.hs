{-# LANGUAGE TemplateHaskell #-}

module Language.Strik.Parsing.Spanned (
    T,
    Spanned (..),
    spannedFromLoc,
    appendSpan,
    prependSpan,

    Span (..),
    spanFromLoc,
    BytesSpan (..),

    Loc (..),
    initialLoc
) where

import           Language.Strik.Prelude

import qualified Language.Parser.Ptera.TH as Ptera


type T = Spanned

data Spanned a = Spanned
    {
        getSpan   :: Span,
        unSpanned :: a
    }
    deriving (Eq, Show, Functor)

instance Ptera.LiftType a => Ptera.LiftType (Spanned a) where
    liftType _ = [t|Spanned $(Ptera.liftType do Proxy @a)|]

instance Semigroup a => Semigroup (Spanned a) where
    sx1 <> sx2 = Spanned
        {
            getSpan = getSpan sx1 <> getSpan sx2,
            unSpanned = unSpanned sx1 <> unSpanned sx2
        }

spannedFromLoc :: Loc -> a -> Spanned a
spannedFromLoc l x = Spanned
    {
        getSpan = spanFromLoc l,
        unSpanned = x
    }

appendSpan :: Spanned a -> Span -> Spanned a
appendSpan sx sp = Spanned
    {
        getSpan = getSpan sx <> sp,
        unSpanned = unSpanned sx
    }

prependSpan :: Span -> Spanned a -> Spanned a
prependSpan sp sx = Spanned
    {
        getSpan = sp <> getSpan sx,
        unSpanned = unSpanned sx
    }

data Span = Span
    {
        beginLoc :: Loc,
        endLoc   :: Loc
    }
    deriving (Eq, Show)

instance Ptera.LiftType Span where
    liftType _ = [t|Span|]

spanFromLoc :: Loc -> Span
spanFromLoc lc = Span
    {
        beginLoc = lc,
        endLoc = lc
    }

instance Semigroup Span where
    sp1 <> sp2 = Span
        {
            beginLoc = min
                do beginLoc sp1
                do beginLoc sp2,
            endLoc = max
                do endLoc sp1
                do endLoc sp2
        }

data BytesSpan = BytesSpan
    {
        bytesIndex  :: Int,
        bytesLength :: Int
    }
    deriving (Eq, Show)

data Loc = Loc
    {
        locBytesPos :: Int,
        locLine     :: Int,
        locCol      :: Int
    }
    deriving (Eq, Ord, Show)

initialLoc :: Loc
initialLoc = Loc
    {
        locBytesPos = 0,
        locLine = 0,
        locCol = 0
    }
