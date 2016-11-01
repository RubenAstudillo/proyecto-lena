module FFT where

import Data.Complex
import Data.Vector as V
import Data.Vector (Vector)
import Numeric.FFT.Vector.Invertible

fft :: Vector (Complex Double) -> Vector (Complex Double)
fft = run dft

-- vector-fftw hace la normalizacion por si mismo
ifft :: Vector (Complex Double) -> Vector (Complex Double)
ifft = run idft
