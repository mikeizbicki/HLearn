{-# LANGUAGE PolyKinds,DataKinds #-}
module HLearn.Metrics.Generic
    where

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

newtype Maximum (xs :: [* -> *]) dp = Maximum dp

type instance Scalar (Maximum '[m] dp) = Scalar (m dp)
type instance Scalar (Maximum (m1 ': m2 ': ms) dp) = Scalar (m1 dp)

-- instance MetricSpace (m dp) => MetricSpace (Maximum '[m] dp) where
--     distance (Maximum a) (Maximum b) = distance a b
