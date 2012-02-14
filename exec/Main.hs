module Main where

--------------------------------------------------------------------------------

--import Codec.Image.DevIL
--import qualified Data.Array.Unboxed as U

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Nebula.Util

--------------------------------------------------------------------------------

main :: IO ()
--main = putStrLn $ show $ (pair 1 2 :: (Integer, Integer))
main =
 do
  putStrLn "hey"
  let datadir = "/home/eric/Dropbox/haskell/Nebula/data/"
  im <- readImage $ datadir ++ "goldfish_girl.jpg"
  putStrLn $ show $ im !.! (10, 10)
--  putStrLn $ show $ U.bounds im
  putStrLn "hi"

