module Utilities where

import Prelude
import Numeric.Matrix
import qualified Data.Map as Map

type ClassTag = Int

appendSample :: MatrixElement a => (ClassTag, Matrix a) -> Map.Map ClassTag (Matrix a) -> Map.Map ClassTag (Matrix a)
appendSample (c, mc) m = 
  let sm = maybe mc (addRow mc) $ Map.lookup c m
  in Map.insert c sm m
  where
    addRow a b = a <-> b

--The number of rows in the matrix must match the number of entries in the class t
--This function divies a given list of ClassTags 
divideSamples :: MatrixElement a => [(ClassTag, Matrix a)] -> Map.Map ClassTag (Matrix a)
divideSamples = foldr appendSample Map.empty

-- Accepts a list of mx1 matrix of data and outputs a mxm covariance matrix estimated using the input
covariance :: (MatrixElement a, Fractional a) => [Matrix a] -> Matrix a
covariance [] = error "No samples provided"
covariance (s:sl) =
  let u = mean (s:sl)
  in scale (foldr (\ x sc -> sc + (x - u) * transpose (x - u)) z (s:sl)) sn
  where
    dim = numRows s
    n = fromIntegral $ length (s:sl)
    sn = fromRational $ (1.0 / n)
    z = matrix (dim, dim) (\(i,j) -> 0)

mean :: (MatrixElement a, Fractional a) => [Matrix a] -> Matrix a
mean (s:sl) = scale (foldr (\x mx -> mx + x) s sl) b
  where
    n = fromIntegral $ length $ s:sl
    b = fromRational $ 1.0 / n
