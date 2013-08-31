>{-# LANGUAGE DataKinds #-}
>{-# LANGUAGE TypeFamilies #-}
>{-# LANGUAGE BangPatterns #-}
>import HLearn.Algebra
>import HLearn.Models.Distributions
>import HLearn.Models.Classifiers.Bayes

>data Person = Person
>   { age           :: !Double
>   , workclass     :: !String
>   , fnlwgt        :: !Double
>   , education     :: !String
>   , educationNum  :: !Double
>   , maritalStatus :: !String
>   , occupation    :: !String
>   , relationship  :: !String
>   , race          :: !String
>   , capitalGain   :: !Double
>   , capitalLoss   :: !Double
>   , hoursPerWeek  :: !Double
>   , nativeCountry :: !String
>   , income        :: !String
>   }

>instance Trainable Person where
>   type GetHList Person = HList '[Double,String,Double,String,Double,String,String,String,String,Double,Double,Double,String]
>   getHList p = age p:::workclass p:::fnlwgt p:::education p:::educationNum p:::maritalStatus p:::occupation p:::relationship p:::race p:::capitalGain p:::capitalLoss p:::hoursPerWeek p:::nativeCountry p:::HNil
