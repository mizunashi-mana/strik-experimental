module Language.Quell.Data.Bag (
    T,
    Bag,
    fromList,
) where

import           Language.Lexer.Tlex.Prelude


type T = Bag

data Bag a
    = EmptyBag
    | UnitBag a
    | BagCons a (Bag a)
    | BagSnoc (Bag a) a
    | TwoBags (Bag a) (Bag a)
    deriving (Show, Functor)

instance Foldable Bag where
    foldl' f z0 b0 = go z0 [] b0 where
        go z s = \case
            EmptyBag        -> foldl' f z s
            UnitBag x       -> foldl' f (f z x) s
            BagCons x b     -> go (f z x) s b
            BagSnoc b x     -> go z (x:s) b
            TwoBags b1 b2   -> go (go z [] b1) s b2

    foldr f z0 b0 = go z0 [] b0 where
        f' x z = f z x

        go z s = \case
            EmptyBag        -> foldl' f' z s
            UnitBag x       -> foldl' f' (f x z) s
            BagCons x b     -> go z (x:s) b
            BagSnoc b x     -> go (f x z) s b
            TwoBags b1 b2   -> go (go z [] b2) s b1

    foldMap f b0 = go b0 where
        go = \case
            EmptyBag        -> mempty
            UnitBag x       -> mempty
            BagCons x b     -> f x <> go b
            BagSnoc b x     -> go b <> f x
            TwoBags b1 b2   -> go b1 <> go b2

instance Eq a => Eq (Bag a) where
    b1 == b2 = toList b1 == toList b2

instance Semigroup (Bag a) where
    EmptyBag        <> b2               = b2
    b1              <> EmptyBag         = b1
    UnitBag x1      <> b2               = BagCons x1 b2
    b1              <> UnitBag x2       = BagSnoc b1 x2
    b1              <> b2               = TwoBags b1 b2

instance Monoid (Bag a) where
    mempty = EmptyBag

instance Applicative Bag where
    pure x = BagCons x EmptyBag

    EmptyBag        <*> _           = mempty
    _               <*> EmptyBag    = mempty
    UnitBag f       <*> mx          = fmap f mx
    mf              <*> UnitBag x   = fmap (\f -> f x) mf
    BagCons f mf1   <*> mx          = fmap f mx <> (mf1 <*> mx)
    BagSnoc mf1 f   <*> mx          = (mf1 <*> mx) <> fmap f mx
    TwoBags mf1 mf2 <*> mx          = (mf1 <*> mx) <> (mf2 <*> mx)

instance Monad Bag where
    mx >>= f = case mx of
        EmptyBag        -> mempty
        UnitBag x       -> f x
        BagCons x mx1   -> f x <> (mx1 >>= f)
        BagSnoc mx1 x   -> (mx1 >>= f) <> f x
        TwoBags mx1 mx2 -> (mx1 >>= f) <> (mx2 >>= f)

fromList :: [a] -> Bag a
fromList xs = foldr
    do \x b -> BagCons x b
    do EmptyBag
    do xs
