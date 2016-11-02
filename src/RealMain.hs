{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
module RealMain where

import Codec.Picture
import Control.Monad.State
import Data.Word
import Data.Complex
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic as G

import Paths_proyecto_lena
import ArrayPoking
import Algoritmo

realMain :: IO ()
realMain =
  do rate : _ <- getArgs
     img@(Image width height _) <- loadImage lena_path
     let red   = V.map fromIntegral (extractColor 0 img)
         green = V.map fromIntegral (extractColor 1 img)
         blue  = V.map fromIntegral (extractColor 2 img)

         calidad = read rate
         redNew   = waveletAlgo calidad red
         greenNew = waveletAlgo calidad green
         blueNew  = waveletAlgo calidad blue

     writePng "./lena_alt.png"
              (nuevaImg width height redNew greenNew blueNew)

lena_path, tiny_path, hunter_path :: FilePath
lena_path = "/home/slack/UTFSM/2016-2/trabajo-roldan-peypo/proyecto-lena/images/Lenna.png"
tiny_path = "/home/slack/UTFSM/2016-2/trabajo-roldan-peypo/proyecto-lena/images/tiny.png"
hunter_path = "/home/slack/UTFSM/2016-2/trabajo-roldan-peypo/proyecto-lena/images/hunter.png"

loadImage :: FilePath -> IO (Image PixelRGB8)
loadImage path =
  do lena <- readImage path
     img  <- either (\s -> putStrLn s >> exitFailure) return lena
     case img of
       ImageRGB8 img_lena -> return img_lena
       _                  -> exitFailure

nuevaImg :: (Complex Double ~ a)
         => Int -> Int -> V.Vector a -> V.Vector a -> V.Vector a
         -> Image PixelRGB8
nuevaImg width height red green blue =
  let combined = combineColors red green blue
   in Image width height . G.map (round . realPart) $ combined
