>{-# LANGUAGE DataKinds #-}
>{-# LANGUAGE TypeFamilies #-}
>{-# LANGUAGE TemplateHaskell #-}

>import Language.Haskell.TH
>import Language.Haskell.TH.Syntax

>import HLearn.Algebra
>import HLearn.Models.Distributions
>import HLearn.Models.Distributions.Multivariate.Internal.CatContainer
>import HLearn.Models.Distributions.Multivariate.Internal.Ignore
>import HLearn.Models.Distributions.Multivariate.Internal.Unital
>import HLearn.Models.Distributions.Multivariate.Internal.TemplateHaskell

>data Person = Person
>   { name :: String
>   , city :: String
>   , job :: Job
>   , age :: Double
>   , salary :: Double
>   }
>   deriving (Read,Show,Eq,Ord)
>
>data Job = Programmer | Manager | Sales
>   deriving (Read,Show,Eq,Ord)

>makeTypeLenses ''Person

-- >test = [| data Test |]

-- >foo ''Person

-- >data Record_name = Record_name
-- >instance IndexName Record_name where type IndexNameOf Record_name = Nat1Box Zero
-- 
-- >data Record_city = Record_city
-- >instance IndexName Record_city where type IndexNameOf Record_city = Nat1Box (Succ Zero)
-- 
-- >data Record_job = Record_job
-- >instance IndexName Record_job where type IndexNameOf Record_job = Nat1Box (Succ (Succ Zero))
-- 
-- >data Record_age = Record_age
-- >instance IndexName Record_age where type IndexNameOf Record_age = Nat1Box (Succ (Succ (Succ Zero)))
-- 
-- >data Record_salary = Record_salary
-- >instance IndexName Record_salary where type IndexNameOf Record_salary = Nat1Box (Succ (Succ (Succ (Succ Zero))))

-- >instance Trainable Person where
-- >   type GetHList Person = HList '[String,String,Job,Double,Double]
-- >   getHList p = name p:::city p:::job p:::age p:::salary p:::HNil

>people = 
>   [ Person "Sally"     "San Francisco" Programmer 22 80000
>   , Person "Billy Bob" "New York"      Programmer 23 75000
>   , Person "Joe Shmoe" "San Francisco" Manager    31 121000
>   , Person "Frank"     "San Francisco" Sales      33 950000
>   , Person "Johnny"    "San Francisco" Programmer 22 80000
>   , Person "Dan"       "New York"      Programmer 22 85000
>   , Person "Smith"     "San Francisco" Sales      32 950000
>   , Person "Joey"      "San Francisco" Programmer 26 82000
>   , Person "Jannette"  "New York"      Programmer 24 73000
>   , Person "Stewart"   "San Francisco" Manager    46 125000
>   , Person "Malcolm"   "San Francisco" Sales      67 910000
>   ]

>person = Person "Mike" "New York" Programmer 27 17000

>dist1 = train people :: Multivariate Person
>   '[ Ignore '[String]
>    , MultiCategorical '[String,Job]

-- >    , Independent Categorical '[Job]

>    , Independent Normal '[Double,Double]
>    ]
>    Double

>dist2 = train people :: Multivariate Person
>   '[ Independent Categorical '[String]
>    , MultiCategorical '[String,Job]
>    , Dependent MultiNormal '[Double,Double]
>    ]
>    Double

Multivariate Person '[ Independent Categorical '[String], MultiCategorical '[String,Job], Dependent MultiNormal '[Double,Double]]Double

>tmpds=["test":::"Sales":::1:::2:::HNil, "test":::"Sales":::2:::2:::HNil, "test":::"Sales":::1:::3:::HNil, "test":::"Sales":::3:::2:::HNil]
>dist = train tmpds :: Multivariate (HList '[String,String,Double,Double])
>  '[ '[CatContainer String]
>   , '[CatContainer String]
>   , Independent Normal '[Double,Double]
>   ]
>   Double
