module Algoritmo where

import Data.Complex
import Control.Monad.State
import Numeric.Statistics (percentile)
import Numeric.LinearAlgebra
import Data.Vector.Storable as VS

import FFT
import ArrayPoking
import Bases

type L2  = Vector (Complex Double)
type Dim = Int

secAnalitica :: L2 -> State Dim (L2, L2)
secAnalitica z =
  do dim <- get
     let zF = fft z
         -- Conjugado reflejado bajo fourier
         uF = conj . fft $ haar1u dim
         vF = conj . fft $ haar1v dim
         x1 = downSampF (zF * vF) dim
         y1 = downSampF (zF * uF) dim
     put (dim `div` 2)
     return (x1, y1)

secSintetica :: (L2, L2) -> State Dim L2
secSintetica (x1, y1) =
  do dim <- get
     let dim_n = dim * 2
         x1' = upSampF x1 dim
         y1' = upSampF y1 dim
         z   = x1' * (fft (haar1v dim_n)) +
               y1' * (fft (haar1u dim_n))
     put $ dim_n
     return (ifft z)

{-
  Idea: Mantener los @k terminos mayores y lo demas setearlos a 0. Es
  molesto contar, asi que veremos un porcentaje.
  rate = 90.0, es decir 90% de calidad, debo de matar 10% de los candidatos.
-}
compresion :: Double -> (L2, L2) -> State Dim (L2, L2)
compresion rate (x1, y1) =
  do dim <- get
     let ix1 = ifft x1
         iy1 = ifft y1
         cut = percentile (100 - rate) . cmap magnitude
                 $ ix1 VS.++ iy1
         go c = if magnitude c < cut then 0 else c
     return $ (fft (VS.map go ix1), fft (VS.map go iy1))

waveletAlgo :: Double -> L2 -> L2
waveletAlgo rate color = evalState go (VS.length color)
  where
    go :: State Dim L2
    go = secAnalitica color >>= compresion rate
         >>= secSintetica
