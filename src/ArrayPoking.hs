{-# language ScopedTypeVariables #-}
module ArrayPoking where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Mutable (MVector, STVector)
import qualified Data.Vector.Storable.Mutable as MV
import Data.Word
import Control.Monad.ST
import Codec.Picture
import Data.Foldable as F

type Offset = Int

extractColor :: Offset -> Image PixelRGB8 -> Vector Word8
extractColor off (Image width height datum) = runST stVec
  where
    -- Cada pixel esta hecho de tres colores, asi que la dimension de
    -- los vectores resultantes se mantiene al filtrar en uno
    dimPixels = (width - 1) * (height - 1)

    stVec :: ST s (Vector Word8)
    stVec =
      do mutvec <- MV.new dimPixels
         let offInd = [0 + off, (3 + off)..(3 * dimPixels)]

         F.forM_ (offInd `zip` [0,1..dimPixels]) $
             \(oi,i) -> MV.write mutvec i (datum V.! oi)

         V.freeze mutvec
