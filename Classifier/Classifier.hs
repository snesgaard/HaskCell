{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Classifier where

import Prelude
import Numeric.Matrix
import Distributions (Distribution, likelihood)
import qualified Data.Map as Map

type ClassTag = Integer
type Prior = Float

data Stub = PositiveStub | NegativeStub

trainStub :: Matrix a -> Stub
trainStub m = PositiveStub

instance Show Stub where
  show PositiveStub = "PositiveStub"
  show NegativeStub = "NegativeStub"

instance Distribution Stub (Matrix a) where
  likelihood m PositiveStub = 1.0
  likelihood m NegativeStub = 0.0

class Classifier c i where
  classify :: i -> c -> Maybe ClassTag --Input should be of dim (m x 1)


-- Implementation code for supervised bayes classifier type
newtype SupervisedBayes d = SupervisedBayes{ classDist :: (Map.Map ClassTag (Prior, d)) } deriving Show --Map containing class label in first index and secodn pair is a classifier primitive with prior

instance (Distribution d i) => Classifier (SupervisedBayes d) i where
  classify m =  fst . Map.foldrWithKey findMax (Nothing, 0.0) . Map.map classLikelihood . classDist
    where
      classLikelihood (p, cd) = (p * likelihood m cd)
      findMax c p (cf, pf) = if p > pf then (Just c, p) else (cf, pf)

incrementSample :: MatrixElement a => (ClassTag, Matrix a) -> Map.Map ClassTag (Matrix a) -> Map.Map ClassTag (Matrix a)
incrementSample (c, mc) m = 
  let sm = maybe mc (addRow mc) $ Map.lookup c m
  in Map.insert c sm m
  where
    addRow a b = a <-> b

--The number of rows in the matrix must match the number of entries in the class tag list
divideSamples :: MatrixElement a => [(ClassTag, Matrix a)] -> Map.Map ClassTag (Matrix a)
divideSamples = foldr incrementSample Map.empty

incrementTag :: ClassTag -> Map.Map ClassTag Integer -> Map.Map ClassTag Integer
incrementTag c m =
  let v = maybe 0 id $ Map.lookup c m
  in Map.insert c (v + 1) m


naivePriorEstimate :: [ClassTag] -> Map.Map ClassTag Prior
naivePriorEstimate c =
  let l = fromIntegral $ length c
  in Map.map (\i -> (fromIntegral i) / l) $ countTag c
  where
    countTag = foldr incrementTag Map.empty

--input needs to be generic here
trainSupervisedBayes :: (Distribution d i, MatrixElement a) => [(ClassTag, Matrix a)] -> (Matrix a -> d) -> SupervisedBayes d
trainSupervisedBayes s t =
  let priors = naivePriorEstimate $ fmap fst s
      samples = divideSamples s
  in SupervisedBayes $ Map.intersectionWith (\p m -> (p, t m)) priors samples


stubBayes :: SupervisedBayes Stub
stubBayes = SupervisedBayes $ Map.fromList [(1, (0.5, PositiveStub)), (2, (0.5, NegativeStub))]
