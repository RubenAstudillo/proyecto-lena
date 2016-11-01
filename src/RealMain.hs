module RealMain where

import Codec.Picture
import System.Exit (exitFailure)

realMain :: IO ()
realMain = loadImage >> return ()

lena_path :: FilePath
lena_path = "/home/slack/UTFSM/2016-2/trabajo-roldan-peypo/proyecto-lena/images/Lenna.png"

loadImage :: IO (Image PixelRGB8)
loadImage =
  do lena <- readImage lena_path
     img  <- either (\s -> putStrLn s >> exitFailure) return lena
     case img of
       ImageRGB8 img_lena -> return img_lena
       _                  -> exitFailure
