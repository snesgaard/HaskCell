{-# LANGUAGE DataKinds #-}
module Classifier where

import Prelude
import Data.Matrix
import qualified Data.Map as Map

type ClassTag = Integer
type Prior = Float

class Distribution d where
  likelihood :: Matrix a -> d -> Float --Outputs the likelihood of an input vector

data Stub = PositiveStub | NegativeStub

trainStub :: Matrix a -> Stub
trainStub m = PositiveStub

instance Show Stub where
  show PositiveStub = "PositiveStub"
  show NegativeStub = "NegativeStub"

instance Distribution Stub where
  likelihood m PositiveStub = 1.0
  likelihood m NegativeStub = 0.0

class Classifier c where
  classify :: Matrix a -> c -> Maybe ClassTag --Input should be of dim (m x 1)

newtype SupervisedBayes d = SupervisedBayes{ classDist :: [(ClassTag, Prior, d)] } deriving Show --Map containing class label in first index and secodn pair is a classifier primitive with prior

instance (Distribution d) => Classifier (SupervisedBayes d) where
  classify m =  fst . foldr findMax (Nothing, 0.0) . fmap classLikelihood . classDist
    where
      classLikelihood (c, p, cd) = (c, p * likelihood m cd)
      findMax (c, p) (cf, pf) = if p > pf then (Just c, p) else (cf, pf)

trainSupervisedBayes :: (Distribution d) => Matrix a -> [ClassTag] -> (Matrix a -> d) -> SupervisedBayes d
trainSupervisedBayes m c t = SupervisedBayes [(1, 0, t m)]

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


stubBayes :: SupervisedBayes Stub
stubBayes = SupervisedBayes [(1, 0.5, PositiveStub), (2, 0.5, NegativeStub)]
