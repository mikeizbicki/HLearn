>{-# LANGUAGE DataKinds #-}
>{-# LANGUAGE TypeFamilies #-}
>import HLearn.Algebra
>import HLearn.Models.Distributions
>import HLearn.Models.Distributions.Multivariate.Internal.CatContainer
>import HLearn.Models.Distributions.Multivariate.Internal.Ignore
>import HLearn.Models.Distributions.Multivariate.Internal.Unital

>data Person = Person
>   { name :: String
>   , city :: String
>   , job :: Job
>   , age :: Double
>   , salary :: Double
>   }
>   deriving (Read,Show,Eq,Ord)

>data Job = Programmer | Manager | Sales
>   deriving (Read,Show,Eq,Ord)

-- >data Sex = Male | Female | Unknown
-- >   deriving (Read,Show,Eq,Ord)

>instance Trainable Person where
>   type GetHList Person = HList '[String,String,Job,Double,Double]
>   getHList p = name p:::city p:::job p:::age p:::salary p:::HNil

>people = 
>   [ Person "Sally"     "San Francisco" Programmer 22 80000

>   , Person "Billy Bob" "New York"      Programmer 23 75000
>   , Person "Joe Shmoe" "San Francisco" Manager    32 120000
>   , Person "Frank"     "San Francisco" Sales      32 950000
>   , Person "Johnny"    "San Francisco" Programmer 22 80000
>   , Person "Dan"       "New York"      Programmer 23 75000
>   , Person "Smith"     "San Francisco" Sales      32 950000
>   , Person "Joey"      "San Francisco" Programmer 22 80000
>   , Person "Jannette"  "New York"      Programmer 23 75000
>   , Person "Stewart"   "San Francisco" Manager    32 120000
>   , Person "Malcolm"   "San Francisco" Sales      32 950000

>   ]

>person = Person "Mike" "New York" Programmer 27 17000

>dist1 = train people :: Multivariate Person
>   '[ Ignore '[String]
>    , Independent Categorical '[String,Job]
>    , Independent Normal '[Double,Double]
>    ]
>    Double

>dist2 = train people :: Multivariate Person
>   '[ Ignore '[String]
>    , MultiCategorical '[String,Job]
>    , Dependent MultiNormal '[Double,Double]
>    ]
>    Double

>tmpds=["test":::"Sales":::1:::2:::HNil, "test":::"Sales":::2:::2:::HNil, "test":::"Sales":::1:::3:::HNil, "test":::"Sales":::3:::2:::HNil]
>dist = train tmpds :: Multivariate (HList '[String,String,Double,Double])
>  '[ '[CatContainer String]
>   , '[CatContainer String]
>   , Independent Normal '[Double,Double]
>   ]
>   Double
