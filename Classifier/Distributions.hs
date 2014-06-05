{-# LANGUAGE MultiParamTypeClasses #-}
module Distributions where

import Numeric.Matrix

class Distribution d where
  likelihood :: Matrix Float -> d -> Float --Outputs the likelihood of an input vector

data Gaussian = Gaussian{
                            covariance :: Matrix Float, --this is an nxn matrix
                            invCovariance :: Maybe (Matrix Float), --containst he inverse of the covariance matrix, if it exists
                            mean :: Matrix Float, --this is a nx1 matrix/vector
                            nFactor :: Float --normalization factor
                          }

instance Distribution Gaussian where
  likelihood x d =
    let xu = (x - u)
    in 1.0
    where
      u = mean d
