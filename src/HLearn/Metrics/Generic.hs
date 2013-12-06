{-# LANGUAGE PolyKinds,DataKinds #-}
module HLearn.Metrics.Generic
    where

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

newtype Maximum (xs :: [* -> *]) dp = Maximum dp

instance HasRing (m dp) => HasRing (Maximum '[m] dp) where
    type Ring (Maximum '[m] dp) = Ring (m dp)

instance 
    ( HasRing (m1 dp)
    , HasRing (Maximum (m2 ': ms) dp)
    , Ring (m1 dp) ~ Ring (Maximum (m2 ': ms) dp)
    ) => HasRing (Maximum (m1 ': m2 ': ms) dp)
        where
    type Ring (Maximum (m1 ': m2 ': ms) dp) = Ring (m1 dp)

-- instance MetricSpace (m dp) => MetricSpace (Maximum '[m] dp) where
--     distance (Maximum a) (Maximum b) = distance a b
