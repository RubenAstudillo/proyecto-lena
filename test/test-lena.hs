module Main where

import Test.Hspec
import Data.Word
import Data.Complex
import Data.Vector.Storable (Vector)
import Data.Vector.Storable as SV
import Codec.Picture
import Data.Monoid
import Control.Monad.State
import Numeric.LinearAlgebra hiding ((<>))

import FFT
import ArrayPoking
import RealMain
import Algoritmo

main :: IO ()
main = hspec $ do
  describe "FFT" $ do
    it "Valor conocido de FFT" $
      fft ex_vec1 `shouldBe` SV.fromList [2, 4 :+ 4, -6, 4 :+ (-4)]

    it "ifft . fft = id" $
      ifft (fft ex_vec1) `shouldBe` ex_vec1

  describe "array poking" $ do
    it "Separa inicio" $ do
      lena_img <- loadImage lena_path
      let red   = extractColor 0 lena_img
          green = extractColor 1 lena_img
          blue  = extractColor 2 lena_img
      SV.take 3 (imageData lena_img) `shouldBe`
        (SV.take 1 red <> SV.take 1 green <> SV.take 1 blue)

    it "Recombinacion" $ do
      lena_img <- loadImage lena_path
      let red   = extractColor 0 lena_img
          green = extractColor 1 lena_img
          blue  = extractColor 2 lena_img
      imageData lena_img `shouldBe` combineColors red green blue

    it "Recombinando %error" $ do
      -- lena_img@(Image width height _) <- loadImage lena_path
      let alt = SV.fromList [4, 2, 3, 7, 10, 8, 10, 14]
          res = evalState (secAnalitica alt >>= secSintetica) 8
      norm_2 (res - alt) / norm_2 alt < 0.1 `shouldBe` True

  -- describe "Pruebas en Lena"
  --   it "Recombinacion Lena" $ do
  --     lena_img <- loadImage lena_path
  --     let red   = extractColor 0 lena_img
  --         green = extractColor 1 lena_img
  --         blue  = extractColor 2 lena_img
  --         go color = evalState (secAnalitica color >>= secSintetica)
  --                              (VS.length color)
  --     imageData lena_img `shouldBe` combineColors red green blue

{- Ejemplo libro pagina 126 -}
ex_vec1 :: Vector (Complex Double)
ex_vec1 = SV.fromList [ 1, 0, -3, 4]
