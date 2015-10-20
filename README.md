# largeword-lens

Provides [traverals](https://hackage.haskell.org/package/lens) for homogeneous `LargeKeys` from the [largeword](https://hackage.haskell.org/package/largeword) package by Dominic Steinitz.
Example usage:

```haskell
import Data.ByteString.Builder
import Data.LargeWord
import Data.LargeWord.Lens
import Data.Lens
import Data.Word

type Word128 = LargeKey Word64 Word64
type Word256 = LargeKey Word128 Word128

word256LE :: Word256 -> Builder
word256LE = foldMapOf leaves word64LE
```
Which, without this package, would have been less neat and more error-prone:

```haskell
import Data.ByteString.Builder
import Data.LargeWord
import Data.Monoid
import Data.Word

type Word128 = LargeKey Word64 Word64
type Word256 = LargeKey Word128 Word128

word256LE :: Word256 -> Builder
word256LE (LargeKey (LargeKey a b) (LargeKey c d)) = word64 a
                                                  <> word64 b
                                                  <> word64 c
                                                  <> word64 d
```
