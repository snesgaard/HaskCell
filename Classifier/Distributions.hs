{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Distributions where

import Numeric.Matrix

class Distribution d i where
  likelihood :: i -> d -> Float --Outputs the likelihood of an input vector

data Gaussian a = Gaussian{
                            covariance :: Matrix a, --this is an nxn matrix
                            invCovariance :: Maybe (Matrix a), --containst he inverse of the covariance matrix, if it exists
                            mean :: Matrix a, --this is a nx1 matrix/vector
                            nFactor :: a --normalization factor
                          }

instance Distribution (Gaussian Float) (Matrix Float) where
  likelihood x d =
    let f = nFactor d
        u = mean d
        ie = maybe (error "Inverse covariance is non-existent") id $ invCovariance d
        xu = x - u
        xut = transpose xu
        pot = at (-0.5 * xut * ie * xu) (0,0)
    in f * (exp pot)
