{-# language TypeFamilies #-}
module Bases where

import qualified Data.Vector.Generic as G
import Data.Complex

import FFT

haar1v, haar1u :: (G.Vector v a, a ~ Complex Double) => Int -> v a
haar1v dim = G.fromList $ [1 / sqrt 2, -1 / sqrt 2] ++ take (dim - 2) (repeat 0)
haar1u dim = G.fromList $ [1 / sqrt 2, 1 / sqrt 2]  ++ take (dim - 2) (repeat 0)
