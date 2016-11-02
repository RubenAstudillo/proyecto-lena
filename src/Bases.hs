{-# language TypeFamilies #-}
module Bases where

import qualified Data.Vector.Generic as G
import Data.Complex

haarV, haarU :: (G.Vector v a, a ~ Complex Double) => Int -> v a
haarV dim = G.fromList $ [1 / sqrt 2, -1 / sqrt 2] ++ take (dim - 2) (repeat 0)
haarU dim = G.fromList $ [1 / sqrt 2,  1 / sqrt 2] ++ take (dim - 2) (repeat 0)
