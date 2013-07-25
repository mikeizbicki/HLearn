{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

import qualified Data.Vector.Unboxed as VU
import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Distributions.Multivariate.Internal.Container
import HLearn.Models.Distributions.Multivariate.Internal.Unital

data Output = A | B
    deriving (Read, Show, Eq, Ord)

data Point = Point
  { _output   :: Output
  , _features :: VU.Vector Double
  } deriving (Read, Show, Eq, Ord)

---------------------------------------
-- these are the instances you'll have to define manualy
data TH_output = TH_output
data TH_features (n::Nat) = TH_features

instance DepIndex Point TH_output where 
    type DepIndexResult Point TH_output = Output
    p # _ = _output p

instance (SingI n) => DepIndex Point (TH_features n) where
    type DepIndexResult Point (TH_features n) = Double
    p # _ = (_features p) VU.! (fromIntegral $ fromSing (sing :: Sing n))

instance HasDepIndex Point where
--     type DepIndexList Point = TH_output ': '[TH_features 0, TH_features 1, TH_features 2, TH_features 3 ]
    type DepIndexList Point = TH_output ': (Replicate 200 (TH_features 0))
--     type DepIndexList Point = '[TH_output,TH_features 0,TH_features 1,TH_features 2,TH_features 3,TH_features 4,TH_features 5,TH_features 6,TH_features 7,TH_features 8,TH_features 9,TH_features 10,TH_features 11,TH_features 12,TH_features 13,TH_features 14,TH_features 15,TH_features 16,TH_features 17,TH_features 18,TH_features 19,TH_features 20,TH_features 21,TH_features 22,TH_features 23,TH_features 24,TH_features 25,TH_features 26,TH_features 27,TH_features 28,TH_features 29,TH_features 30,TH_features 31,TH_features 32,TH_features 33,TH_features 34,TH_features 35,TH_features 36,TH_features 37,TH_features 38,TH_features 39,TH_features 40,TH_features 41,TH_features 42,TH_features 43,TH_features 44,TH_features 45,TH_features 46,TH_features 47,TH_features 48,TH_features 49,TH_features 50,TH_features 51,TH_features 52,TH_features 53,TH_features 54,TH_features 55,TH_features 56,TH_features 57,TH_features 58,TH_features 59,TH_features 60,TH_features 61,TH_features 62,TH_features 63,TH_features 64,TH_features 65,TH_features 66,TH_features 67,TH_features 68,TH_features 69,TH_features 70,TH_features 71,TH_features 72,TH_features 73,TH_features 74,TH_features 75,TH_features 76,TH_features 77,TH_features 78,TH_features 79,TH_features 80,TH_features 81,TH_features 82,TH_features 83,TH_features 84,TH_features 85,TH_features 86,TH_features 87,TH_features 88,TH_features 89,TH_features 90,TH_features 91,TH_features 92,TH_features 93,TH_features 94,TH_features 95,TH_features 96,TH_features 97,TH_features 98,TH_features 99,TH_features 100,TH_features 101,TH_features 102,TH_features 103,TH_features 104,TH_features 105,TH_features 106,TH_features 107,TH_features 108,TH_features 109,TH_features 110,TH_features 111,TH_features 112,TH_features 113,TH_features 114,TH_features 115,TH_features 116,TH_features 117,TH_features 118,TH_features 119,TH_features 120,TH_features 121,TH_features 122,TH_features 123,TH_features 124,TH_features 125,TH_features 126,TH_features 127,TH_features 128,TH_features 129,TH_features 130,TH_features 131,TH_features 132,TH_features 133,TH_features 134,TH_features 135,TH_features 136,TH_features 137,TH_features 138,TH_features 139,TH_features 140,TH_features 141,TH_features 142,TH_features 143,TH_features 144,TH_features 145,TH_features 146,TH_features 147,TH_features 148,TH_features 149,TH_features 150,TH_features 151,TH_features 152,TH_features 153,TH_features 154,TH_features 155,TH_features 156,TH_features 157,TH_features 158,TH_features 159,TH_features 160,TH_features 161,TH_features 162,TH_features 163,TH_features 164,TH_features 165,TH_features 166,TH_features 167,TH_features 168,TH_features 169,TH_features 170,TH_features 171,TH_features 172,TH_features 173,TH_features 174,TH_features 175,TH_features 176,TH_features 177,TH_features 178,TH_features 179,TH_features 180,TH_features 181,TH_features 182,TH_features 183,TH_features 184,TH_features 185,TH_features 186,TH_features 187,TH_features 188,TH_features 189,TH_features 190,TH_features 191,TH_features 192,TH_features 193,TH_features 194,TH_features 195,TH_features 196,TH_features 197,TH_features 198,TH_features 199]

type NB = Multivariate Point
            '[ MultiCategorical   '[Output]
-- -- 	         , Independent Normal '[Double,Double,Double,Double] 
	         , Independent Normal (Replicate 200 Double)
-- 	         , Dependent MultiNormal (Replicate 500 Double)
--  	         , Dependent MultiNormal '[Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double]
	         ]
	         Double

mkIndepNormal :: Int -> String
mkIndepNormal 0 = "Unital Double"
mkIndepNormal i = "Container Normal Double ("++(mkIndepNormal (i-1))++") Double" 

xs = [ Point A $ VU.replicate 500 10
     , Point B $ VU.fromList [1 .. 500]
     , Point A $ VU.fromList [(-500) .. (-1)]
     , Point B $ VU.replicate 500 55
     ]

-- dist = train xs :: NB
