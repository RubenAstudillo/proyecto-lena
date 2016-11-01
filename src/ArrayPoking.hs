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

downFourier :: forall a v. (G.Vector v a, Fractional a)
            => v a -> Int -> v a
downFourier vec dimN = G.create stVec
  where
    -- Convencion Libro
    dimM = dimN `div` 2

    stVec :: forall s. ST s (G.Mutable v s a)
    stVec =
      do mutvec <- GM.new dimM
         F.forM_ [0..(dimM - 1)] $ \i ->
           GM.write mutvec i (go vec i)
         return mutvec

    -- Teorema de downsampling y transformada de Fourier
    go :: v a -> Int -> a
    go z i = 0.5 * (z G.! i + z G.! (i + dimM))
