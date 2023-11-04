module Language.Quell.Data.Bag (
    T,
    Bag,
    empty,
    singleton,
    fromList,
) where

import           Language.Quell.Prelude hiding (empty, singleton)

import qualified Data.Foldable          as Foldable


type T = Bag

data Bag a
    = Empty
    | Unit a
    | List [a]
    | Append (Bag a) (Bag a)
    deriving (Show, Functor, Foldable.Foldable, Traversable)

empty :: Bag a
empty = Empty

singleton :: a -> Bag a
singleton x = Unit x

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
    Empty   <> b2       = b2
    b1      <> Empty    = b1
    Unit x1 <> Unit x2  = List [x1, x2]
    Unit x1 <> List xs2 = List do x1:xs2
    b1      <> b2       = Append b1 b2

instance Monoid (Bag a) where
    mempty = empty

instance Applicative Bag where
    pure x = singleton x

    Empty          <*> _      = empty
    _              <*> Empty  = empty
    Unit f         <*> mx     = fmap f mx
    mf             <*> Unit x = fmap (\f -> f x) mf
    List fs        <*> mx     = ofoldMap (\f -> fmap f mx) fs
    Append mf1 mf2 <*> mx     = (mf1 <*> mx) <> (mf2 <*> mx)

instance Monad Bag where
    mx >>= f = case mx of
        Empty          -> empty
        Unit x         -> f x
        List xs        -> ofoldMap (\x -> f x) xs
        Append mx1 mx2 -> (mx1 >>= f) <> (mx2 >>= f)

instance GrowingAppend (Bag a)

instance SemiSequence (Bag a) where
    type Index (Bag a) = Int

    intersperse sep b0 = ofoldr
        do \x -> \case
            Empty  -> Unit x
            Unit y -> List [x,sep,y]
            List b -> List do x:sep:b
            b      -> Append
                do List [x,sep]
                do b
        do Empty
        do otoList b0

    reverse b0 = ofoldl'
        do \b x -> cons x b
        do Empty
        do otoList b0

    find cond b0 = go [] b0 where
        go bs = \case
            Empty           -> goAll bs
            Unit x
                | cond x    -> Just x
                | otherwise -> goAll bs
            List xs         -> case find cond xs of
                r@Just{} -> r
                Nothing  -> goAll bs
            Append b1 b2    -> go (b2:bs) b1

        goAll = \case
            []   -> Nothing
            b:bs -> go bs b

    sortBy cmp xs = fromList do sortBy cmp do otoList xs

    cons x = \case
        Empty  -> Unit x
        Unit y -> List [x,y]
        List b -> List do x:b
        b      -> Append
            do Unit x
            do b

    snoc b x = case b of
        Empty  -> Unit x
        Unit y -> List [y,x]
        _      -> Append
            do b
            do Unit x

instance MonoPointed (Bag a) where
    opoint = singleton

instance IsSequence (Bag a) where
    fromList xs = ofoldr
        do \x b -> cons x b
        do Empty
        do xs

    splitWhen f xs = [ fromList l | l <- splitWhen f do otoList xs ]
