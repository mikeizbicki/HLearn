If our learning model is a functor, then we get asymptotically faster preprocessing of data.

{-# LANGUAGE RebindableSyntax #-}

> import Control.ConstraintKinds 
> import Prelude hiding (fmap,return,(>>=),(>>))

> import GHC.Float

> import HLearn.Algebra
> import HLearn.Models.Distributions

 ifThenElse = undefined

> data Marble = Red | Green | Blue | White
>   deriving (Read,Show,Eq,Ord)

> xs = [ Red,Green,Red,Blue,Green,Red,Blue,White ]

> m1 = train xs :: Categorical Double Marble

> m2 = fmap show m1

> forgetBlue :: Marble -> Maybe Marble
> forgetBlue Blue = Nothing
> forgetBlue marble = Just marble

> doublered :: (Num prob) => Marble -> Categorical prob Marble
> doublered Red = 2 .* train1dp Red 
> doublered dp  = train1dp dp

> forgetBlueM :: (Num prob) => Marble -> Categorical prob Marble
> forgetBlueM Blue = mempty
> forgetBlueM dp = train1dp dp

> addnoise :: (Fractional prob) => Marble -> Categorical prob Marble
> addnoise dp = 0.6 .* train1dp dp <> 0.1 .* train [ Red,Green,Blue,White ]

> redgreennoise :: (Fractional prob) => Marble -> Categorical prob Marble
> redgreennoise Red   = trainW [(0.7,Red),(0.3,Green)]
> redgreennoise Green = trainW [(0.1,Red),(0.9,Green)]
> redgreennoise dp    = train1dp dp 

 m3 :: Categorical Double Marble
 m3 = do
   dp <- train xs
   dp' <- addnoise dp
   return dp'


> ys = [1..1000] :: [Double]

> kde1 = train ys :: KDE Gaussian 10 Double 

> kde1_float = fmap double2Float kde1

> kde_squared = fmap (^2) kde1
