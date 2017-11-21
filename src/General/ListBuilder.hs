{-# LANGUAGE CPP #-}

module General.ListBuilder(
    ListBuilder, runListBuilder, newListBuilder
    ) where

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif
import Data.Monoid
import Prelude()

data ListBuilder a
    = Zero
    | One a
    | Add (ListBuilder a) (ListBuilder a)

#if MIN_VERSION_base(4,9,0)
instance Semigroup (ListBuilder a) where
    (<>) = mappend
#endif

instance Monoid (ListBuilder a) where
    mempty = Zero
    mappend Zero x = x
    mappend x Zero = x
    mappend x y = Add x y

newListBuilder :: a -> ListBuilder a
newListBuilder = One

runListBuilder :: ListBuilder a -> [a]
runListBuilder x = f x []
    where
        f Zero acc = []
        f (One x) acc = x : acc
        f (Add x y) acc = f x (f y acc)
