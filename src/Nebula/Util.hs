module Nebula.Util where

--------------------------------------------------------------------------------

import Codec.Image.DevIL (Word8)
import qualified Codec.Image.DevIL as DevIL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Array.Unboxed as AU

--------------------------------------------------------------------------------

pair :: a -> b -> (a, b)
pair x y = (x, y)

(..-) :: (Num t, Enum t) =>
         t -> t -> [t]
start ..- stop = [start .. stop - 1]

-- The outer is boxed because an unboxed vector cannot contain another vector.
-- I.e., VU.Vector is not an instance of VU.Unbox.
type Matrix a = V.Vector (VU.Vector a)

(!.!) :: VU.Unbox a =>
         Matrix a -> (Int, Int) -> a
matrix !.! (y, x) = (matrix V.! y) VU.! x

type RGB = (Word8, Word8, Word8)

-- The opposite of |concat|, breaking a list into sublists of a given length.
unconcat :: Int -> [a] -> [[a]]
unconcat _ [] = [[]]
unconcat num list = (take num list) : (unconcat num (drop num list))

matrixFromRGBUArray :: AU.UArray (Int, Int, Int) Word8 -> Matrix RGB
matrixFromRGBUArray array = V.fromList $ map VU.fromList rows
 where (_, (_, xMax, 3)) = AU.bounds array
       toTuple [r, g, b] = (r, g, b)
       rgbs = map toTuple $ unconcat 3 $ AU.elems array
       rows = unconcat xMax rgbs

sliceMatrix :: (VU.Unbox a) =>
               ((Int, Int), (Int, Int)) -> Matrix a -> Matrix a
sliceMatrix ((yMin, yNum), (xMin, xNum)) matrix =
 V.map (VU.slice xMin xNum) $ V.slice yMin yNum matrix

patches :: (VU.Unbox a) =>
           Int -> Matrix a -> [Matrix a]
patches patchWidth matrix = map ((flip sliceMatrix) matrix) slices
 where (ymax, xmax) = (V.length matrix, VU.length (V.head matrix))
       ys = 0 ..- (ymax - patchWidth)
       xs = 0 ..- (xmax - patchWidth)
       slices = [((y, patchWidth), (x, patchWidth)) | y <- ys, x <- xs]

readImage :: FilePath -> IO (Matrix RGB)
readImage path = do DevIL.ilInit
                    image <- DevIL.readImage path
                    return $ matrixFromRGBUArray image

write

--readImage :: FilePath -> IO (AU.UArray (Int, Int, Int) D.Word8)


--type Int3 = (Int, Int, Int)
--
--sliceArray3 :: (Int3, Int3) ->
--               U.Array Int3 e ->
--               U.Array Int3 e
--sliceArray3 (lower@(ymin, xmin, zmin), upper@(ymax, xmax, zmax)) array =
--  U.array bounds $ zip (U.range bounds) values
-- where bounds = ((0, 0, 0), (ymax - ymin, xmax - xmin, zmax - zmin))
--       values = map (array U.!) $ U.range (lower, upper)
--
--patches :: Int -> U.Array Int3 e -> [U.Array Int3 e]
--patches patchWidth array = map (flip . sliceArray3 array) boundss
-- where ((ymin, xmin, _), (ymax, xmax, _)) = U.bounds array
--       ys = [ymin .. ymax - patchWidth + 1]


--readImage ::
