{-# LANGUAGE FlexibleInstances #-}

module Utilities where

import Prelude
import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra.Util
import Numeric.Container
import Foreign.Storable
import qualified Data.Map as Map

type ClassTag = Int

appendSample :: (Element a) => (ClassTag, Vector a) -> Map.Map ClassTag (Matrix a) -> Map.Map ClassTag (Matrix a)
appendSample (c, v) m = 
  let sm = maybe (asRow v) (addRow v) $ Map.lookup c m
  in Map.insert c sm m
  where
    addRow a b = fromRows $ a:(toRows b)


-- Accepts a list of mx1 matrix of data and outputs a mxm covariance matrix estimated using the input
covariance :: Matrix Double -> Matrix Double
covariance m =
  let r = toRows m
      u = mean r
      n = fromIntegral $ rows m
  in (sum $ map (\v -> outer v v) $ map (\v -> v - u) r) / n


--It is assumed that the dimensionality of each vector is identical
mean :: [Vector Double] -> Vector Double
mean v = (sum v) / (fromIntegral $ length v)

vecInt2Frac :: (Integral a, Element a, Fractional b, Element b) => Vector a -> Vector b
vecInt2Frac = mapVector (\i -> fromIntegral i)
