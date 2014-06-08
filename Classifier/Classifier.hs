{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Classifier where

import Prelude
import Numeric.Matrix
import qualified Data.Map as Map

type ClassTag = Int

class Classifier i c where
  classify :: i -> c -> Maybe ClassTag --Input should be of dim (m x 1)

data LDA a = LDA {
                  getAxis :: Matrix a, --only a mx1 matrix/vector should be allowed
                  getProjection :: Matrix a, -- mxm matrix
                  getThreshold :: a
                  }

instance (MatrixElement a, Ord a) => Classifier (Matrix a) (LDA a) where
  classify i c =
    let p = getProjection c
        t = getThreshold c
        v = at (p*i) (1,1)
    in Just $ if v > t then 1 else 0


