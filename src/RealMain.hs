module RealMain where

import Codec.Picture
import System.Exit (exitFailure, exitSuccess)

import Paths_proyecto_lena
import ArrayPoking
import qualified Data.Vector.Storable as V

realMain :: IO ()
realMain =
  do img <- loadImage lena_path
     let red = extractColor 0 img
     print red

lena_path, tiny_path :: FilePath
lena_path = "/home/slack/UTFSM/2016-2/trabajo-roldan-peypo/proyecto-lena/images/Lenna.png"
tiny_path = "/home/slack/UTFSM/2016-2/trabajo-roldan-peypo/proyecto-lena/images/tiny.png"

loadImage :: FilePath -> IO (Image PixelRGB8)
loadImage path =
  do lena <- readImage path
     img  <- either (\s -> putStrLn s >> exitFailure) return lena
     case img of
       ImageRGB8 img_lena -> return img_lena
       _                  -> exitFailure
