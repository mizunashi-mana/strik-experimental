module Language.Quell.Data.Bag (
    T,
    Bag,
    fromList,
) where

import           Language.Quell.Prelude
import qualified Data.Foldable as Foldable


type T = Bag

data Bag a
    = EmptyBag
    | UnitBag a
    | ListBag [a]
    | BagCons a (Bag a)
    | BagSnoc (Bag a) a
    | TwoBags (Bag a) (Bag a)
    deriving (Show, Functor, Foldable.Foldable, Traversable)

type instance Element (Bag a) = a

instance MonoFunctor (Bag a) where
    omap = fmap

instance MonoFoldable (Bag a) where
    ofoldMap = Foldable.foldMap
    ofoldr = Foldable.foldr
    ofoldl' = Foldable.foldl'
    otoList = Foldable.toList

instance MonoTraversable (Bag a) where
    otraverse = traverse

instance MonoTraversableM (Bag a) where
    omapM = mapM

instance Eq a => Eq (Bag a) where
    b1 == b2 = otoList b1 == otoList b2

instance Semigroup (Bag a) where
    EmptyBag        <> b2               = b2
    b1              <> EmptyBag         = b1
    UnitBag x1      <> ListBag xs2      = ListBag do x1:xs2
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
    ListBag fs      <*> mx          = ofoldMap (\f -> fmap f mx) fs
    BagCons f mf1   <*> mx          = fmap f mx <> (mf1 <*> mx)
    BagSnoc mf1 f   <*> mx          = (mf1 <*> mx) <> fmap f mx
    TwoBags mf1 mf2 <*> mx          = (mf1 <*> mx) <> (mf2 <*> mx)

instance Monad Bag where
    mx >>= f = case mx of
        EmptyBag        -> mempty
        UnitBag x       -> f x
        ListBag xs      -> ofoldMap (\x -> f x) xs
        BagCons x mx1   -> f x <> (mx1 >>= f)
        BagSnoc mx1 x   -> (mx1 >>= f) <> f x
        TwoBags mx1 mx2 -> (mx1 >>= f) <> (mx2 >>= f)

instance GrowingAppend (Bag a)

instance SemiSequence (Bag a) where
    type Index (Bag a) = Int

    intersperse sep b0 = ofoldr
        do \x -> \case
            EmptyBag    -> UnitBag x
            b           -> BagCons sep do BagCons x b
        do EmptyBag
        do otoList b0

    reverse b0 = ofoldl'
        do \b x -> case b of
            EmptyBag    -> UnitBag x
            _           -> BagCons x b
        do EmptyBag
        do otoList b0

    find cond b0 = go [] b0 where
        go bs = \case
            EmptyBag        -> goAll bs
            UnitBag x
                | cond x    -> Just x
                | otherwise -> goAll bs
            ListBag xs      -> case find cond xs of
                r@(Just x)  -> r
                Nothing     -> goAll bs
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

    sortBy cmp xs = fromList do sortBy cmp do otoList xs

    cons x b = BagCons x b
    snoc b x = BagSnoc b x

instance MonoPointed (Bag a) where
    opoint = pure

instance IsSequence (Bag a) where
    fromList xs = ofoldr
        do \x b -> BagCons x b
        do EmptyBag
        do xs

    splitWhen f xs = [ fromList l | l <- splitWhen f do otoList xs ]
