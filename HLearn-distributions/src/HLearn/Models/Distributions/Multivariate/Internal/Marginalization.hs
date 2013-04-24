{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | All multivariate distributions should implement methods to marginalize
module HLearn.Models.Distributions.Multivariate.Internal.Marginalization
    where

import GHC.TypeLits
import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- type classes
    
class Marginalize index dist where
    type Margin index dist
    getMargin :: index -> dist -> Margin index dist
    
    type MarginalizeOut index dist 
    marginalizeOut :: index -> dist -> MarginalizeOut index dist
    