module RealMain where

import Codec.Picture
import System.Exit (exitFailure, exitSuccess)

import Paths_proyecto_lena
import ArrayPoking

realMain :: IO ()
realMain = loadImage >>= return . extractColor 0 >> exitSuccess

lena_path :: FilePath
lena_path = "./images/Lenna.png"

loadImage :: IO (Image PixelRGB8)
loadImage =
  do lena <- readImage lena_path
     img  <- either (\s -> putStrLn s >> exitFailure) return lena
     case img of
       ImageRGB8 img_lena -> return img_lena
       _                  -> exitFailure
