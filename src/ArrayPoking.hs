{-# language ScopedTypeVariables #-}
module ArrayPoking where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV
import Data.Vector.Mutable (MVector, STVector)
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Word
import Control.Monad.ST
import Data.STRef
import Codec.Picture
import Data.Foldable as F

type Offset = Int

extractColor :: Offset -> Image PixelRGB8 -> Vector Word8
extractColor off (Image width height datum) = runST stVec
  where
    -- Cada pixel esta hecho de tres colores, asi que la dimension de
    -- los vectores resultantes se mantiene al filtrar en uno
    dimPixels = width * height

    stVec :: ST s (Vector Word8)
    stVec =
      do mutvec <- MV.new dimPixels
         let offInd = [0 + off, (3 + off)..(3 * dimPixels - 1)]

         F.forM_ (offInd `zip` [0,1..(dimPixels - 1)]) $
             \(oi,i) -> MV.write mutvec i (datum SV.! oi)

         SV.freeze mutvec

combineColors :: (G.Vector v a) => v a -> v a -> v a -> v a
combineColors red green blue = G.create stVec
  where
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
