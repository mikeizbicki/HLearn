module HLearn.Models.Regression.Common
    where

import HLearn.Models.Classifiers.Common

data Coord ring = Coord 
    { coordx::ring
    , coordy::ring
    }

instance Labeled (Coord ring) where
    type Label (Coord ring) = ring
    type Attributes (Coord ring) = ring
    getLabel = coordy
    getAttributes = coordx

