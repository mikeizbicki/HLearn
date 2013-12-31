{-# LANGUAGE DataKinds,PolyKinds,QuasiQuotes,RankNTypes,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances #-}

module HLearn.Models.Regression
    where

import Data.List
import qualified Data.Foldable as F
import qualified Data.Semigroup as SG
import Numeric

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra hiding (Function(..))
import HLearn.Models.Classifiers.Common hiding (Regression (..))
import HLearn.Models.Regression.Parsing

-------------------------------------------------------------------------------
-- data types

newtype Regression expr dp = Regression { unRegression :: AddUnit1 (Reg expr) dp }

deriving instance (Element (Ring dp), Field (Ring dp), Container Vector (Ring dp)) => Monoid (Regression expr dp)

instance (Show (Ring dp), RealFloat (Ring dp), Element (Ring dp), SingI expr) => Show (Regression expr dp) where
--     show m = show $ fromSing (sing :: Sing expr)
    show m = "y = " ++ (concat $ intersperse " + " $ map prettyPrint $ filter shouldPrint $ zip coefL termL)
        where
            shouldPrint (c,t) = abs c > 0.000001
            
            prettyPrint (c,CCon t) = showGFloat (Just 4) (c * fromRational t) ""
            prettyPrint (c,t) = showGFloat (Just 4) c "" ++ ppShowTerm t

            termL = fromSing (sing :: Sing expr)
            coefL = concat . toLists $ beta $ unUnit1 $ unRegression $ m

data Reg (expr:: [TermT]) dp = Reg 
    { beta :: Matrix (Ring dp)
    , rawM  :: !(Matrix (Ring dp))
    , rawV  :: !(Vector (Ring dp))
    , _numdp :: !(Ring dp)
    }

mkReg :: 
    ( Field (Ring dp)
    , Element (Ring dp)
    , Num (Ring dp)
    ) => Ring dp -> Vector (Ring dp) -> Matrix (Ring dp) -> Reg expr dp
mkReg numdp' rawV' rawM' = Reg
    { beta = (inv $ scale (1/numdp') rawM')  LA.<> (scale (1/numdp') $ asColumn rawV') 
    , rawM = rawM'
    , rawV = rawV'
    , _numdp = numdp'
    }

-------------------------------------------------------------------------------
-- algebra

instance 
    ( Element (Ring dp)
    , Container Vector (Ring dp)
    , Field (Ring dp)
    , Fractional (Ring dp)
    ) => SG.Semigroup (Reg expr dp) where
    r1 <> r2 = mkReg
        (_numdp r1 + _numdp r2)
        (rawV r1 `LA.add` rawV r2)
        (rawM r1 `LA.add` rawM r2)

-------------------------------------------------------------------------------
-- training

instance 
    ( Num (Label dp)
    , Num (Attributes dp)
    , Label dp ~ Attributes dp
    , Ring dp ~ Attributes dp
    , Element (Attributes dp)
    , Container Vector (Attributes dp)
    , LA.Product (Attributes dp)
    , Labeled dp
    , Field (Attributes dp)
    , SingI expr
    ) => HomTrainer (Regression expr dp)
        where
    type Datapoint (Regression expr dp) = dp
    
    train1dp dp = Regression $ UnitLift1 $ mkReg
        (1)
        (scale (getLabel dp) (dp2vec (sing::Sing expr) $ getAttributes dp))
        ((asColumn $ dp2vec (sing::Sing expr) $ getAttributes dp) LA.<> (asRow $ dp2vec (sing::Sing expr) $ getAttributes dp))

{-# INLINE dp2vec #-}
-- dp2vec :: (Num dp, Storable dp, SingI expr) => Sing expr -> dp -> Vector dp
-- dp2vec _ dp = LA.fromList [1,dp,dp*dp] 

dp2vec :: forall expr dp.
    ( Floating dp
    , Storable dp
    , SingI expr
    ) => Sing (expr :: [TermT]) -> dp -> Vector dp
dp2vec _ dp = LA.fromList $ map (flip evalTerm dp) $ fromSing (sing :: Sing expr)

-------------------------------------------------------------------------------
-- model use

-- train xs :: Regression Linear [expr| x + log x + x^2|] Double

reg :: forall dp expr. 
    ( Attributes dp ~ Label dp
    , Ring dp ~ Attributes dp
    , HasRing dp
    , Floating (Label dp)
    , LA.Product (Label dp)
    , Storable (Label dp)
    , SingI expr
    ) => Attributes dp -> Regression expr dp -> Label dp
reg dp (Regression (UnitLift1 r)) = ((asRow $ dp2vec (sing::Sing expr) dp) LA.<> (beta r)) @@> (0,0)

-- testdp = [(1,1),(2,1),(3,1),(6,5),(4,5)]
-- testdp = [(1,1),(2,2),(3,4)]
testdp = [(2,1),(5,2),(10,3),(17,4)]
-- testm = train testdp :: Regression '[VarT] (Double,Double)
-- testm = train testdp :: Regression '[ConT (1/1),VarT,BinT Pow (ConT (2/1)) VarT] (Double,Double)
-- testm = train testdp :: Regression '[ConT (1/1),VarT,BinT Mult VarT VarT] (Double,Double)
testm = train testdp :: Regression [expr| 1 + x + x^2 + log x |] (Double,Double)
