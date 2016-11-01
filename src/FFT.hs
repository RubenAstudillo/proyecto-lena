{-# language TypeFamilies #-}
module FFT where

import Data.Complex
import Data.Vector.Generic
import Numeric.FFT.Vector.Invertible

fft :: (Vector v a, a ~ Complex Double) => v a -> v a
fft = run dft

-- vector-fftw hace la normalizacion por si mismo
ifft :: (Vector v a, a ~ Complex Double) => v a -> v a
ifft = run idft
