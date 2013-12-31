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

import HLearn.Algebra
import HLearn.Models.Classifiers.Common hiding (Regression (..))
import HLearn.Models.Regression.Parsing

import HLearn.Algebra.Types.Reflection
import Data.Constraint
import Data.Reflection

-------------------------------------------------------------------------------
-- data types

newtype Regression expr dp = Regression { unRegression :: AddUnit1 (Reg expr) dp }

---------------------------------------

class Param_expr m => ValidParams m

instance Param_expr (Regression expr dp) => ValidParams (Regression expr dp)

class Param_expr m where
    param_expr :: m -> [Term]

instance SingI expr => Param_expr (Regression (Just expr) dp) where
    param_expr _ = fromSing (sing :: Sing expr)

instance Param_expr (Regression Nothing dp) where
    param_expr m = mp_expr $ modelparams $ unUnit1 $ unRegression m

instance ReifiableConstraint Param_expr where
    data Def Param_expr m = Param_expr { param_expr_ :: m -> [Term] }
    reifiedIns = Sub Dict

instance SetParam Param_expr (Regression Nothing dp) where
    data DefParam Param_expr (Regression Nothing dp) = 
           DefParam_expr { unDefParam_expr :: [Term] }
    setParam p a = using (Param_expr $ \x -> unDefParam_expr p) a 

str2expr = DefParam_expr . parseExprSimple

instance Reifies s (Def Param_expr m) => Param_expr (Lift Param_expr m s) where 
    param_expr m = param_expr_ (reflect m) (lower m)

data family ModelParams (m :: *)
data instance ModelParams (Reg expr dp) = ModelParams_Reg { mp_expr :: [Term] }

instance SingI (Maybe a)

---------------------------------------

deriving instance (Element (Ring dp), Field (Ring dp), Container Vector (Ring dp), ValidParams (Regression expr dp)) => Monoid (Regression expr dp)

instance (Show (Ring dp), RealFloat (Ring dp), Element (Ring dp)) => Show (Regression expr dp) where 
--     show _ = "show"
-- instance (Show (Ring dp), RealFloat (Ring dp), Element (Ring dp), ValidParams (Regression expr dp)) => Show (Regression expr dp) where
    show m = "y = " ++ (concat $ intersperse " + " $ map prettyPrint $ filter shouldPrint $ zip coefL termL)
        where
            param_expr m = mp_expr $ modelparams $ unUnit1 $ unRegression m
            shouldPrint (c,t) = c /= c || abs c > 0.000001
            
            prettyPrint (c,CCon t) = showGFloat (Just 4) (c * fromRational t) ""
            prettyPrint (c,t) = showGFloat (Just 4) c "" ++ ppShowTerm t

            termL = param_expr m 
            coefL = concat . toLists $ beta $ unUnit1 $ unRegression $ m

data Reg (expr:: Maybe [TermT]) dp = Reg 
    { beta :: Matrix (Ring dp)
    , rawM  :: !(Matrix (Ring dp))
    , rawV  :: !(Vector (Ring dp))
    , _numdp :: !(Ring dp)
    , modelparams :: !(ModelParams (Reg expr dp))
    }

mkReg :: forall expr dp.
    ( Field (Ring dp)
    , Element (Ring dp)
    , Num (Ring dp)
    , ValidParams (Regression expr dp)
    ) => Ring dp -> Vector (Ring dp) -> Matrix (Ring dp) -> Reg expr dp
mkReg numdp' rawV' rawM' = Reg
    { beta = (inv $ scale (1/numdp') rawM')  LA.<> (scale (1/numdp') $ asColumn rawV') 
    , rawM = rawM'
    , rawV = rawV'
    , _numdp = numdp'
    , modelparams = ModelParams_Reg $ param_expr (error "poop1" :: Regression expr dp)
    }

-------------------------------------------------------------------------------
-- algebra

instance 
    ( Element (Ring dp)
    , Container Vector (Ring dp)
    , Field (Ring dp)
    , Fractional (Ring dp)
    , ValidParams (Regression expr dp)
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
    , ValidParams (Regression expr dp)
    ) => HomTrainer (Regression expr dp)
        where
    type Datapoint (Regression expr dp) = dp
    
    train1dp dp = Regression $ UnitLift1 $ mkReg
        (1)
        (scale (getLabel dp) (dp2vec expr $ getAttributes dp))
        ((asColumn $ dp2vec expr $ getAttributes dp) LA.<> (asRow $ dp2vec expr $ getAttributes dp))
        where
            expr = param_expr (error "poop2" :: Regression expr dp)

{-# INLINE dp2vec #-}
dp2vec :: forall dp.
    ( Floating dp
    , Storable dp
    ) => [Term] -> dp -> Vector dp
dp2vec expr dp = LA.fromList $ map (flip evalTerm dp) expr

-- dp2vec :: forall expr dp.
--     ( Floating dp
--     , Storable dp
--     , SingI expr
--     ) => Sing (expr :: [TermT]) -> dp -> Vector dp
-- dp2vec _  = dp2vec' (fromSing (sing :: Sing expr))

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
    , ValidParams (Regression expr dp)
    ) => Attributes dp -> Regression expr dp -> Label dp
reg dp (Regression (UnitLift1 r)) = ((asRow $ dp2vec expr dp) LA.<> (beta r)) @@> (0,0)
    where
        expr = param_expr (error "poop3" :: Regression expr dp)

-- testdp = [(1,1),(2,1),(3,1),(6,5),(4,5)]
-- testdp = [(1,1),(2,2),(3,4)]
testdp = [(2,1),(5,2),(10,3),(17,4)]
-- testm = train testdp :: Regression '[VarT] (Double,Double)
-- testm = train testdp :: Regression '[ConT (1/1),VarT,BinT Pow (ConT (2/1)) VarT] (Double,Double)
-- testm = train testdp :: Regression '[ConT (1/1),VarT,BinT Mult VarT VarT] (Double,Double)
testm = train testdp :: Regression (Just [expr| 1 + x + x^2 + log x |]) (Double,Double)
