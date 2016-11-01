module Algoritmo where

import Data.Complex
import Control.Monad.Reader
import Numeric.LinearAlgebra

import FFT
import ArrayPoking
import Bases

type L2 = Vector (Complex Double)

seccionAnalitica :: L2 -> Reader Int (L2, L2)
seccionAnalitica z =
  do dim <- ask
     let z_hat = fft z
         -- Conjugado reflejado bajo fourier
         u_hat_refl = conj . fft $ haar1u dim
         v_hat_refl = conj . fft $ haar1v dim
     x1 <- reader $ downFourier (z_hat * v_hat_refl)
     y1 <- reader $ downFourier (z_hat * u_hat_refl)
     return (x1, y1)
