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

    sortBy cmp b0 = go b0 where
        go = \case
            EmptyBag        -> EmptyBag
            UnitBag x       -> UnitBag x
            BagCons x b     -> merge [UnitBag x] [go b]
            BagSnoc b x     -> merge [go b] [UnitBag x]
            TwoBags b1 b2   -> merge [go b1] [go b2]

        merge []                        []                          = EmptyBag
        merge []                        (b2:bs2)                    = b2 <> merge [] bs2
        merge (b1:bs1)                  []                          = b1 <> merge [] bs1
        merge (EmptyBag:bs1)            bs2                         = merge bs1 bs2
        merge bs1                       (EmptyBag:bs2)              = merge bs1 bs2
        merge (TwoBags b1 b2:bs1)       bs2                         = merge (b1:b2:bs1) bs2
        merge bs1                       (TwoBags b1 b2:bs2)         = merge bs1 (b1:b2:bs2)
        merge bs1@(UnitBag x1:bs1')     bs2@(UnitBag x2:bs2')       = case cmp x1 x2 of
            LT  -> BagCons x1 do merge bs1' bs2
            GT  -> BagCons x2 do merge bs1 bs2'
            EQ  -> BagCons x1 do BagCons x2 do merge bs1' bs2'
        merge bs1@(UnitBag x1:bs1')     bs2@(BagCons x2 b2:bs2') = case cmp x1 x2 of
            LT  -> BagCons x1 do merge bs1' bs2
            GT  -> BagCons x2 do merge bs1 (b2:bs2')
            EQ  -> BagCons x1 do BagCons x2 do merge bs1' (b2:bs2')
        merge bs1@(UnitBag x1:bs1')     (b2@(BagSnoc b2' x2):bs2') = case cmp x1 x2 of
            LT  -> merge [UnitBag x1] [b2'] <> merge bs1' (UnitBag x2:bs2')
            GT  -> b2 <> merge bs1 bs2'
            EQ  -> BagSnoc b2 x1 <> merge bs1' bs2'
        merge bs1@(BagCons x1 b1:bs1')  bs2@(BagCons x2 b2:bs2') = case cmp x1 x2 of
            LT  -> BagCons x1 do merge (b1:bs1') bs2
            GT  -> BagCons x2 do merge bs1 (b2:bs2')
            EQ  -> BagCons x1 do BagCons x2 do merge (b1:bs1') (b2:bs2')
        merge bs1@(BagCons x1 b1:bs1')  (b2@(BagSnoc b2' x2):bs2') = case cmp x1 x2 of
            LT  -> merge bs1 (b2':UnitBag x2:bs2')
            GT  -> b2 <> merge bs1 bs2'
            EQ  -> BagSnoc b2 x1 <> merge (b1:bs1') bs2'
        merge (b1@(BagSnoc b1' x1):bs1')  (b2@(BagSnoc b2' x2):bs2') = case cmp x1 x2 of
            LT  -> merge [b1] [b2'] <> merge bs1' (UnitBag x2:bs2')
            GT  -> merge [b1'] [b2] <> merge (UnitBag x1:bs1') bs2'
            EQ  -> BagSnoc (BagSnoc (merge [b1'] [b2']) x1) x2 <> merge bs1' bs2'
        merge bs1 bs2 = merge bs2 bs1

    cons x b = BagCons x b
    snoc b x = BagSnoc b x

instance MonoPointed (Bag a) where
    opoint = pure

instance IsSequence (Bag a) where
    fromList :: [a] -> Bag a
    fromList xs = foldr
        do \x b -> BagCons x b
        do EmptyBag
        do xs
