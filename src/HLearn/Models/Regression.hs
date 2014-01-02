{-# LANGUAGE DataKinds,PolyKinds,QuasiQuotes,RankNTypes,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances,TemplateHaskell #-}

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
import qualified Language.Haskell.TH as TH
import Debug.Trace

-------------------------------------------------------------------------------
-- data types

data family ModelParams (m :: *)
data instance ModelParams (Reg method expr dp) = ModelParams_Reg { mp_expr :: [Term] }

newtype Regression method expr dp = Regression { unRegression :: AddUnit1 (Reg method expr) dp }
newtype Regression2 expr2 dp2 = Regression2 { unRegression2 :: AddUnit1 (Reg Nothing expr2) dp2 }

data Method = Linear | Ridge 
data instance Sing (s::Method) = SMethod Method
instance SingI 'Linear where sing = SMethod Linear
instance SingI 'Ridge where sing = SMethod Ridge
instance SingE (Kind::Method) Method where   
    fromSing (SMethod r) = r

type family Set_expr (r:: *) (expr:: [TermT]) :: *
type instance Set_expr (Regression method exprold dp) expr = Regression method (Just expr) dp

type DefRegression dp = Regression Nothing Nothing dp

data Reg (method::Maybe Method) (expr:: Maybe [TermT]) dp = Reg 
    { beta :: Matrix (Ring dp)
    , rawM  :: !(Matrix (Ring dp))
    , rawV  :: !(Vector (Ring dp))
    , _numdp :: !(Ring dp)
    , modelparams :: !(ModelParams (Reg method expr dp))
    }

mkParams ''Regression

class Param_expr m => ValidParams m

instance Param_expr (Regression method expr dp) => ValidParams (Regression method expr dp)

instance Param_expr (Regression method Nothing dp) where
    param_expr m = mp_expr $ modelparams $ unUnit1 $ unRegression m

str2expr = SetParam_expr . parseExprSimple

test_Nothing = setParam (str2expr "1 + x^2") $ train [(1,0),(2,1),(3,2)] :: Regression (Just Linear) Nothing (Double,Double)
test_Just = train [(1,0),(2,1),(3,2)] :: Regression (Just Linear) (Just [expr| 1 + x |]) (Double,Double)
test_Just2 = train [(1,0),(2,1),(3,2)] :: (DefRegression (Double,Double)) `Set_expr` [expr| 1 + x|]


deriving instance (Element (Ring dp), Field (Ring dp), Container Vector (Ring dp), ValidParams (Regression method expr dp)) => Monoid (Regression method expr dp)

instance (Show (Ring dp), RealFloat (Ring dp), Element (Ring dp)) => Show (Regression method expr dp) where 
--     show _ = "show"
-- instance (Show (Ring dp), RealFloat (Ring dp), Element (Ring dp), ValidParams (Regression method expr dp)) => Show (Regression method expr dp) where
    show m = "y = " ++ (concat $ intersperse " + " $ map prettyPrint $ filter shouldPrint $ zip coefL termL)
        where
            param_expr m = mp_expr $ modelparams $ unUnit1 $ unRegression m
            shouldPrint (c,t) = c /= c || abs c > 0.000001
            
            prettyPrint (c,CCon t) = showGFloat (Just 4) (c * fromRational t) ""
            prettyPrint (c,t) = showGFloat (Just 4) c "" ++ ppShowTerm t

            termL = param_expr m 
            coefL = concat . toLists $ beta $ unUnit1 $ unRegression $ m


mkReg :: forall method expr dp.
    ( Field (Ring dp)
    , Element (Ring dp)
    , Num (Ring dp)
    , ValidParams (Regression method expr dp)
    ) => Ring dp -> Vector (Ring dp) -> Matrix (Ring dp) -> Reg method expr dp
mkReg numdp' rawV' rawM' = Reg
    { beta = (inv $ scale (1/numdp') rawM')  LA.<> (scale (1/numdp') $ asColumn rawV') 
    , rawM = rawM'
    , rawV = rawV'
    , _numdp = numdp'
    , modelparams = ModelParams_Reg $ param_expr (error "poop1" :: Regression method expr dp)
    }

-------------------------------------------------------------------------------
-- algebra

instance 
    ( Element (Ring dp)
    , Container Vector (Ring dp)
    , Field (Ring dp)
    , Fractional (Ring dp)
    , ValidParams (Regression method expr dp)
    ) => SG.Semigroup (Reg method expr dp) where
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
    , ValidParams (Regression method expr dp)
    ) => HomTrainer (Regression method expr dp)
        where
    type Datapoint (Regression method expr dp) = dp
    
    train1dp dp = Regression $ UnitLift1 $ mkReg
        (1)
        (scale (getLabel dp) (dp2vec expr $ getAttributes dp))
        ((asColumn $ dp2vec expr $ getAttributes dp) LA.<> (asRow $ dp2vec expr $ getAttributes dp))
        where
            expr = param_expr (error "poop2" :: Regression method expr dp)

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

reg :: forall method expr dp. 
    ( Attributes dp ~ Label dp
    , Ring dp ~ Attributes dp
    , HasRing dp
    , Floating (Label dp)
    , LA.Product (Label dp)
    , Storable (Label dp)
    , ValidParams (Regression method expr dp)
    ) => Attributes dp -> Regression method expr dp -> Label dp
reg dp (Regression (UnitLift1 r)) = ((asRow $ dp2vec expr dp) LA.<> (beta r)) @@> (0,0)
    where
        expr = param_expr (error "poop3" :: Regression method expr dp)

-- testdp = [(1,1),(2,1),(3,1),(6,5),(4,5)]
-- testdp = [(1,1),(2,2),(3,4)]
testdp = [(2,1),(5,2),(10,3),(17,4)]
-- testm = train testdp :: Regression '[VarT] (Double,Double)
-- testm = train testdp :: Regression '[ConT (1/1),VarT,BinT Pow (ConT (2/1)) VarT] (Double,Double)
-- testm = train testdp :: Regression '[ConT (1/1),VarT,BinT Mult VarT VarT] (Double,Double)
-- testm = train testdp :: Regression (Just [expr| 1 + x + x^2 + log x |]) (Double,Double)
