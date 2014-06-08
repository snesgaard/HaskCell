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

instance MatrixElement a => (Distribution a) (Matrix a) where
    likelihood x d = 1.0

class Constant c where
  getConstant :: c

instance Constant (Matrix Float) where
  getConstant = matrix (3,3) (\(i,j)->1.0)
