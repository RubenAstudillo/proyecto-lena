module Algoritmo where

import Prelude as P
import Data.Complex
import Control.Monad.State
import Numeric.Statistics (percentile)
import Numeric.LinearAlgebra
import Data.Vector.Storable as VS hiding (modify, null)

import FFT
import ArrayPoking
import Bases

type L2  = Vector (Complex Double)
type Dim = Int
type Niveles = Int

secAnalitica :: Niveles -> L2 -> State [L2] ()
secAnalitica n zF =
  do let dim = VS.length zF
         uF  = conj . fft $ haarU dim
         vF  = conj . fft $ haarV dim
         x1  = downSampF (zF * vF) dim
         y1  = downSampF (zF * uF) dim

     -- [y3, x3, x2, x1]
     -- modify (x1 :)
     if n <= 1
       -- Haz nada, este es el ultimo nivel
       then modify (zF :)
       --
       else modify (x1 :) >> secAnalitica (n - 1) y1

secSintetica :: Niveles -> State [L2] L2
secSintetica n =
  do y2 : x2 : rest <- get
     let dim = VS.length y2
         dim_n = dim * 2
         x2' = upSampF x2 dim
         y2' = upSampF y2 dim
         y1  = x2' * (fft (haarV dim_n)) +
               y2' * (fft (haarU dim_n))
     put (y1 : rest)

     if null rest || n <= 1
       then return y1
       else secSintetica (n - 1)

{-
  Idea: Mantener los @k terminos mayores y lo demas setearlos a 0. Es
  molesto contar, asi que veremos un porcentaje.
  rate = 90.0, es decir 90% de calidad, debo de matar 10% de los
  candidatos.
-}
compresion :: Double -> State [L2] ()
compresion rate =
  do vec <- get
     let cut  = percentile (100 - rate) . sortIP
                . cmap magnitude $ VS.concat vec

         go c = if magnitude c < cut then 0 else c
     put $ P.map (VS.map go) vec

waveletAlgo :: Double -> Niveles -> Niveles -> L2 -> L2
waveletAlgo calidad div recon color = evalState go []
  where
    go :: State [L2] L2
    go = return (fft color)
         >>= secAnalitica div
         -- >> modify (P.map ifft)
         >> compresion calidad
         -- >> modify (P.map fft)
         >> secSintetica recon
         >>= return . ifft
