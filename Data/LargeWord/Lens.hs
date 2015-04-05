{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Data.LargeWord.Lens
    ( leaves
    , leaf
    ) where

import Data.Word
import Data.LargeWord
import Control.Lens

class Homogenous s a where
    leaves :: Traversal' s a
    leaf :: Int -> Traversal' s a

instance Homogenous Word8  Word8  where
    leaves = id
    leaf 0 = id
    leaf _ = const pure

instance Homogenous Word16 Word16 where
    leaves = id
    leaf 0 = id
    leaf _ = const pure

instance Homogenous Word32 Word32 where
    leaves = id
    leaf 0 = id
    leaf _ = const pure

instance Homogenous Word64 Word64 where
    leaves = id
    leaf 0 = id
    leaf _ = const pure

instance (Homogenous l a, Homogenous r a) => Homogenous (LargeKey l r) a where
    leaves f (LargeKey l r) = LargeKey <$> leaves f l <*> leaves f r
    leaf i f (LargeKey l r) =
        if i < count
        then LargeKey <$> leaf i f l <*> pure r
        else LargeKey l <$> leaf (i - count) f r
      where count = lengthOf (leaves :: Traversal' l a) l
