{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
module ArrayPoking where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Word
import Control.Monad.ST
import Data.STRef
import Codec.Picture
import Data.Foldable as F
import Control.Category ((>>>))
import qualified Data.Vector.Algorithms.Intro as Al

type Offset = Int

extractColor :: forall a v. (G.Vector v a, a ~ Word8)
             => Offset -> Image PixelRGB8 -> v a
extractColor off (Image width height datum) = G.create stVec
  where
    -- Cada pixel esta hecho de tres colores, asi que la dimension de
    -- los vectores resultantes se mantiene al filtrar en uno
    dimPixels = width * height

    stVec :: forall s. ST s (G.Mutable v s Word8)
    stVec =
      do mutvec <- GM.new dimPixels
         let offInd = [0 + off, (3 + off)..(3 * dimPixels - 1)]

         F.forM_ (offInd `zip` [0,1..(dimPixels - 1)]) $
             \(oi,i) -> GM.write mutvec i (datum G.! oi)

         return mutvec

combineColors :: forall a v. (G.Vector v a) => v a -> v a -> v a -> v a
combineColors red green blue = G.create stVec
  where
    stVec :: forall s. ST s (G.Mutable v s a)
    stVec =
      do mutvec <- GM.new (G.length red * 3)
         ref    <- newSTRef 0

         F.forM_ [0..(G.length red - 1)] $ \c ->
           do i <- readSTRef ref
              GM.write mutvec  i      (red G.! c)
              GM.write mutvec (i + 1) (green G.! c)
              GM.write mutvec (i + 2) (blue G.! c)
              writeSTRef ref  (i + 3)

         return mutvec

downSampF :: forall a v. (G.Vector v a, Fractional a)
          => v a -> Int -> v a
downSampF vec dim_n = G.create stVec
  where
    -- Convencion Libro
    dim_m = dim_n `div` 2

    stVec :: forall s. ST s (G.Mutable v s a)
    stVec =
      do mutvec <- GM.new dim_m
         F.forM_ [0..(dim_m - 1)] $ \i ->
           GM.write mutvec i (go vec i)
         return mutvec

    -- Teorema de downsampling y transformada de Fourier
    go :: v a -> Int -> a
    go z i = 0.5 * (z G.! i + z G.! (i + dim_m))

downSamp :: forall v a. (G.Vector v a) => v a -> Int ->  v a
downSamp vec dim_n = G.create stVec
  where
    -- Convencion Libro
    dim_m = dim_n `div` 2

    stVec :: forall s. ST s (G.Mutable v s a)
    stVec =
      do mutvec <- GM.new dim_m
         F.forM_ [0..(dim_m - 1)] $ \i ->
           GM.write mutvec i (vec G.! (2 * i))
         return mutvec

upSampF :: forall a v. (G.Vector v a, Num a) => v a -> Int -> v a
upSampF vec dim_m = G.create stVec
  where
    dim_n = 2 * dim_m

    stVec :: forall s. ST s (G.Mutable v s a)
    stVec =
      do mutvec <- GM.replicate dim_n 0
         F.forM_ [0,1..(dim_n - 1)] $
           \i -> GM.write mutvec i (vec G.! (mod i dim_m))
         return mutvec

-- Sort in place
sortIP :: forall a v. (G.Vector v a, Ord a) => v a -> v a
sortIP vec = runST stVec
  where
    stVec :: forall s. ST s (v a)
    stVec = do mutvec <- G.thaw vec
               Al.sort mutvec
               G.freeze mutvec
