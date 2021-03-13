module Language.Quell.Data.Bag (
    T,
    Bag,
    fromList,
) where

import           Language.Quell.Prelude
import           Data.Foldable


type T = Bag

data Bag a
    = EmptyBag
    | UnitBag a
    | BagCons a (Bag a)
    | BagSnoc (Bag a) a
    | TwoBags (Bag a) (Bag a)
    deriving (Show, Functor, Foldable, Traversable)

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

type instance Element (Bag a) = a

instance MonoFunctor (Bag a) where
    omap = fmap

instance MonoFoldable (Bag a) where
    ofoldMap = foldMap
    ofoldr = foldr
    ofoldl' = foldl'

instance MonoTraversable (Bag a) where
    otraverse = traverse

instance MonoTraversableM (Bag a) where
    omapM = mapM

instance GrowingAppend (Bag a)

instance SemiSequence (Bag a) where
    type Index (Bag a) = Int

    intersperse sep b0 = foldr
        do \x -> \case
            EmptyBag    -> UnitBag x
            b           -> BagCons sep do BagCons x b
        do EmptyBag
        do toList b0

    reverse b0 = foldl'
        do \b x -> case b of
            EmptyBag    -> UnitBag x
            _           -> BagCons x b
        do EmptyBag
        do toList b0

    find cond b0 = go [] b0 where
        go bs = \case
            EmptyBag        -> goAll bs
            UnitBag x
                | cond x    -> Just x
                | otherwise -> goAll bs
            BagCons x b
                | cond x    -> Just x
                | otherwise -> go bs b
            BagSnoc b x     -> go (addB x bs) b
            TwoBags b1 b2   -> go (b2:bs) b1

        goAll = \case
            []      -> Nothing
            b:bs    -> go bs b

        addB x = \case
            []      -> [UnitBag x]
            b:bs    -> BagCons x b:bs

    sortBy cmp xs = fromList do sortBy cmp do toList xs

    cons x b = BagCons x b
    snoc b x = BagSnoc b x

instance MonoPointed (Bag a) where
    opoint = pure

instance IsSequence (Bag a) where
    fromList xs = foldr
        do \x b -> BagCons x b
        do EmptyBag
        do xs

    splitWhen f xs = [ fromList l | l <- splitWhen f do toList xs ]
