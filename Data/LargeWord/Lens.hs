{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Data.LargeWord.Lens
    ( Homogeneous(..)
    ) where

import Control.Applicative
import Data.LargeWord
import Data.Monoid
import Data.Word

type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

-- | If @'Homogeneous' s a@, then all holes in 's' are 'a''s.
-- This allows us to traverse over them.
class Homogeneous s a where
    -- | A 'Traversal' over a the holes of 's'.
    leaves :: Traversal' s a
    -- | A 'Traversal' over a specific hole of 's'.
    leaf :: Int -> Traversal' s a
    -- | Create an 's' filled with a given value.
    populate :: a -> s

instance Homogeneous Word8 Word8  where
    leaves = id
    leaf 0 = id
    leaf _ = const pure
    populate = id

instance Homogeneous Word16 Word16 where
    leaves = id
    leaf 0 = id
    leaf _ = const pure
    populate = id

instance Homogeneous Word32 Word32 where
    leaves = id
    leaf 0 = id
    leaf _ = const pure
    populate = id

instance Homogeneous Word64 Word64 where
    leaves = id
    leaf 0 = id
    leaf _ = const pure
    populate = id

instance (Homogeneous l a, Homogeneous r a) => Homogeneous (LargeKey l r) a where
    leaves f (LargeKey l r) = LargeKey <$> leaves f l <*> leaves f r
    leaf i f (LargeKey l r) =
        if i < count
        then LargeKey <$> leaf i f l <*> pure r
        else LargeKey l <$> leaf (i - count) f r
      where count = getSum . getConst $ (leaves :: Traversal' l a) (Const . const (Sum 1)) l
    populate x = LargeKey (populate x) (populate x)
