module Main where

import Test.Hspec
import Data.Vector (Vector)
import Data.Vector as V
import Data.Word
import Data.Complex

import FFT

main :: IO ()
main = hspec $ do
  describe "FFT" $ do
    it "Valor conocido de FFT" $
      fft ex_vec1 `shouldBe` V.fromList [2, 4 :+ 4, -6, 4 :+ (-4)]

    it "ifft . fft = id" $
      ifft (fft ex_vec1) `shouldBe` ex_vec1

{- Ejemplo libro pagina 126 -}
ex_vec1 :: Vector (Complex Double)
ex_vec1 = V.fromList [ 1, 0, -3, 4]
