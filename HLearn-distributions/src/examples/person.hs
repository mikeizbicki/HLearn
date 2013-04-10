data Person = Person
    { name :: String
    , employer :: String
    , age :: Double
    , salary :: Double
    }
    
instance Trainable Person where
    type GetHList Person = HList '[String,String,Double,Double]
    getHList p = name p:::employer p:::age p:::salary p:::HNil
    
ps = [ Person "Mike" "UCR" 27 18000
     , Person "Jack" "UCR" 28 19000
     ]
     
testps = train ps :: Multivariate Person
    [ MultiCategorical '[String,String]
    , Independent Normal '[Double,Double]
    ]
    Double

testps2 = train ps :: Multivariate Person
    [ Independent Categorical '[String,String]
    , Independent Normal '[Double,Double]
    ]
    Double
