module Main where

--------------------------------------------------------------------------------

import Codec.Image.DevIL
import Data.Array.Unboxed

import Nebula.Util

--------------------------------------------------------------------------------

main :: IO ()
--main = putStrLn $ show $ (pair 1 2 :: (Integer, Integer))
main =
 do
  ilInit
  let datadir = "/home/eric/Dropbox/haskell/Nebula/data/"
  im <- readImage $ datadir ++ "goldfish_girl.jpg"
  putStrLn $ show $ im ! (10, 10, 3)
  putStrLn "hi"

