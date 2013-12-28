{-# LANGUAGE DataKinds,PolyKinds,QuasiQuotes #-}

module HLearn.Models.Regression
    where

import Data.List
import qualified Data.Foldable as F
import qualified Data.Semigroup as SG

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra hiding (Function(..))
import HLearn.Models.Classifiers.Common hiding (Regression (..))
import HLearn.Models.Regression.Parsing

-------------------------------------------------------------------------------
-- data types

type Regression eq dp = AddUnit1 (Reg eq) dp

data Reg (eq:: [TermT]) dp = Reg 
    { beta :: Matrix (Ring dp)
    , rawM  :: !(Matrix (Ring dp))
    , rawV  :: !(Vector (Ring dp))
    , _numdp :: !(Ring dp)
    }

mkReg :: 
    ( Field (Ring dp)
    , Element (Ring dp)
    , Num (Ring dp)
    ) => Ring dp -> Vector (Ring dp) -> Matrix (Ring dp) -> Reg eq dp
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
    ) => SG.Semigroup (Reg eq dp) where
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
    , SingI eq
    ) => HomTrainer (AddUnit1 (Reg eq) dp)
        where
    type Datapoint (AddUnit1 (Reg eq) dp) = dp
    
    train1dp dp = UnitLift1 $ mkReg
        (1)
        (scale (getLabel dp) (dp2vec (sing::Sing eq) $ getAttributes dp))
        ((asColumn $ dp2vec (sing::Sing eq) $ getAttributes dp) LA.<> (asRow $ dp2vec (sing::Sing eq) $ getAttributes dp))

{-# INLINE dp2vec #-}
-- dp2vec :: (Num dp, Storable dp, SingI eq) => Sing eq -> dp -> Vector dp
-- dp2vec _ dp = LA.fromList [1,dp,dp*dp] 

dp2vec :: forall eq dp.
    ( Floating dp
    , Storable dp
    , SingI eq
    ) => Sing (eq :: [TermT]) -> dp -> Vector dp
dp2vec _ dp = LA.fromList $ map (flip evalTerm dp) $ fromSing (sing :: Sing eq)

-------------------------------------------------------------------------------
-- model use

-- train xs :: Regression Linear [expr| x + log x + x^2|] Double

reg :: forall dp eq. 
    ( Attributes dp ~ Label dp
    , Ring dp ~ Attributes dp
    , HasRing dp
    , Floating (Label dp)
    , LA.Product (Label dp)
    , Storable (Label dp)
    , SingI eq
    ) => Attributes dp -> Regression eq dp -> Label dp
reg dp (UnitLift1 r) = ((asRow $ dp2vec (sing::Sing eq) dp) LA.<> (beta r)) @@> (0,0)

-- testdp = [(1,1),(2,1),(3,1),(6,5),(4,5)]
-- testdp = [(1,1),(2,2),(3,4)]
testdp = [(2,1),(5,2),(10,3),(17,4)]
-- testm = train testdp :: Regression '[VarT] (Double,Double)
-- testm = train testdp :: Regression '[ConT (1/1),VarT,BinT Pow (ConT (2/1)) VarT] (Double,Double)
-- testm = train testdp :: Regression '[ConT (1/1),VarT,BinT Mult VarT VarT] (Double,Double)
testm = train testdp :: Regression [expr| 1 + x + x^2 + log x |] (Double,Double)
