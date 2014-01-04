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

data Method = Linear | Ridge 
data instance Sing (s::Method) = SMethod Method
instance SingI 'Linear where sing = SMethod Linear
instance SingI 'Ridge where sing = SMethod Ridge
instance SingE (Kind::Method) Method where   
    fromSing (SMethod r) = r

---------------------------------------

data Regression (method::Maybe Method) (expr:: Maybe [TermT]) dp = Regression 
    { beta :: Matrix (Ring dp)
    , rawM  :: !(Matrix (Ring dp))
    , rawV  :: !(Vector (Ring dp))
    , _numdp :: !(Ring dp)
    , modelparams :: ModelParams (Regression method expr dp)
    }

-- mkParams ''Regression

class Param_expr m where param_expr :: m -> [Term]

instance ReifiableConstraint Param_expr where
    data Def Param_expr m = Def_Param_expr { param_expr_ :: m -> [Term] }
    reifiedIns = Sub Dict

instance Reifies s (Def Param_expr a) => Param_expr (ConstraintLift Param_expr a s) where
    param_expr a = param_expr_ (reflect a) (lower a)

instance SingI expr => Param_expr (Regression method (Just expr) dp) where
    param_expr _ = fromSing (sing :: Sing expr)

instance SetParam Param_expr (Regression method Nothing dp) where
    data DefParam Param_expr (Regression method Nothing dp) = 
            SetParam_expr { unSetParam_expr :: [Term] } --Def Param_expr (Regression method Nothing dp) }
    setParam p a = using (Def_Param_expr $ \x -> unSetParam_expr p) a

-- setParam' :: Def p a -> ((p
setParam' :: 
    ( SetParam p m
    , p ~ Param_expr
    , m ~ Regression method Nothing dp
    ) => DefParam p m -> ((p m) => m) -> (m -> a) -> a
-- setParam' :: DefParam Param_expr (Regression method Nothing dp) -> ((Param_expr m) => m) -> (m -> a) -> a
setParam' p m f = using' (Def_Param_expr $ \x -> unSetParam_expr p) m f

-- instance Param_expr (Regression method Nothing dp) where
--     param_expr m = mp_expr $ modelparams m 

---------------------------------------

data family ModelParams (m :: *)
data instance ModelParams (Regression Nothing Nothing dp) = ModelParams_Reg_Empty { mp_method :: Method, mp_expr :: [Term] }
data instance ModelParams (Regression (Just method) Nothing dp) = ModelParams_Reg_1 { mp_expr1 :: [Term] }
data instance ModelParams (Regression Nothing (Just expr) dp) = ModelParams_Reg_2 { mp_expr2 :: [Term] }
data instance ModelParams (Regression (Just method) (Just expr) dp) = ModelParams_Reg_Full ()

class CreateModelParams m where
    createModelParams :: m -> ModelParams m

instance Param_expr (Regression method expr dp) => CreateModelParams (Regression method expr dp) where
    createModelParams _ = undefined -- ModelParams_Reg $ param_expr (undefined :: Regression method Nothing dp)

type family Set_expr (r:: *) (expr:: [TermT]) :: *
type instance Set_expr (Regression method exprold dp) expr = Regression method (Just expr) dp

type DefRegression dp = Regression Nothing Nothing dp

data Regression' method expr dp = Regression'
    { model :: !(Regression method expr dp)
    , params :: !(ModelParams (Regression method expr dp))
    }

-- instance Show (Regression' method Nothing dp) where
--     show r = setParam' (SetParam_expr $ mp_expr $ params r) (model r) show
--     show r = show $ setParam (SetParam_expr $ mp_expr $ params r) $ model r
--     show r = show $ model r

-- instance Param_expr (Regression' method Nothing dp) where
--     param_expr m = mp_expr $ params m 

instance SingI expr => Param_expr (Regression' method (Just expr) dp) where    
    param_expr _ = fromSing (sing :: Sing expr)

instance 
    ( Param_expr (Regression method expr dp)
    , Element (Ring dp)
    , Container Vector (Ring dp)
    , Field (Ring dp)
    , Fractional (Ring dp)
    ) => Monoid (Regression' method expr dp) where
    mempty = undefined
    mappend a b = Regression'
        { model = model a <> model b
        , params = params a
        }

instance
    ( Param_expr (Regression method expr dp)
    , CreateModelParams (Regression method expr dp)
    , Element (Ring dp)
    , Container Vector (Ring dp)
    , Field (Ring dp)
    , Fractional (Ring dp)
    , HomTrainer (Regression method expr dp)
    ) => HomTrainer (Regression' method expr dp) 
        where
    type Datapoint (Regression' method expr dp) = Datapoint (Regression method expr dp)
    train xs = Regression'
        { model = train xs
        , params = createModelParams (undefined :: Regression method expr dp)
        }

instance HasRing dp => HasRing (Regression method expr dp) where
    type Ring (Regression method expr dp) = Ring dp

instance HasRing dp => NumDP (Regression method expr dp) where
    numdp = _numdp

instance 
    ( Labeled dp
    , Label dp ~ Attributes dp
    , Ring dp ~ Attributes dp
    , Param_expr (Regression method expr dp)
    , LA.Product (Attributes dp)
    , HasRing dp
    , Floating (Attributes dp)
    , HomTrainer (Regression method expr dp)
    ) => Classifier (Regression method expr dp) 
        where
    classify = flip reg

---------------------------------------

-- expr = SetParam_expr
str2expr = SetParam_expr . parseExprSimple

test_Nothing = setParam (str2expr "1 + x^2") $ train [(1,0),(2,1),(3,2)] :: Regression (Just Linear) Nothing (Double,Double)
test_Just = train [(1,0),(2,1),(3,2),(4,4),(5,5)] :: Regression (Just Linear) (Just [expr| 1 + x + x^2 |]) (Double,Double)
test_Just' = train [(1,0),(2,1),(3,2),(4,4),(5,5)] :: Regression' (Just Linear) (Just [expr| 1 + x + x^2 |]) (Double,Double)
-- test_Just2 = train [(1,0),(2,1),(3,2)] :: (DefRegression (Double,Double)) `Set_expr` [expr| 1 + x|]


instance 
    ( Show (Ring dp)
    , RealFloat (Ring dp)
    , Element (Ring dp)
    , Param_expr (Regression method expr dp)
    ) => Show (Regression method expr dp) where 
    show m = "y = " ++ (concat $ intersperse " + " $ map prettyPrint $ filter shouldPrint $ zip coefL termL)
        where
            shouldPrint (c,t) = c /= c || abs c > 0.000001
            
            prettyPrint (c,CCon t) = showGFloat (Just 4) (c * fromRational t) ""
            prettyPrint (c,t) = showGFloat (Just 4) c "" ++ ppShowTerm t

            termL = param_expr m 
            coefL = concat . toLists $ beta m 


mkReg :: forall method expr dp.
    ( Field (Ring dp)
    , Element (Ring dp)
    , Num (Ring dp)
    , Param_expr (Regression method expr dp)
    ) => Ring dp -> Vector (Ring dp) -> Matrix (Ring dp) -> Regression method expr dp
mkReg numdp' rawV' rawM' = Regression
    { beta = (inv $ scale (1/numdp') rawM')  LA.<> (scale (1/numdp') $ asColumn rawV') 
    , rawM = rawM'
    , rawV = rawV'
    , _numdp = numdp'
    , modelparams = createModelParams (undefined :: Regression method expr dp)
    }

-------------------------------------------------------------------------------
-- algebra

instance 
    ( Element (Ring dp)
    , Container Vector (Ring dp)
    , Field (Ring dp)
    , Fractional (Ring dp)
    , Param_expr (Regression method expr dp)
    ) => Monoid (Regression method expr dp) 
        where
    mempty = mkReg 0 (fromList $ replicate numdim 0) (numdim><numdim $ replicate (numdim*numdim) 0)
        where
            numdim = length $ param_expr (undefined :: Regression method expr dp)

    mappend r1 r2 = mkReg
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
    , Param_expr (Regression method expr dp)
    ) => HomTrainer (Regression method expr dp)
        where
    type Datapoint (Regression method expr dp) = dp
    
    train1dp dp = mkReg
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

-------------------------------------------------------------------------------
-- model use

reg :: forall method expr dp. 
    ( Attributes dp ~ Label dp
    , Ring dp ~ Attributes dp
    , HasRing dp
    , Floating (Label dp)
    , LA.Product (Label dp)
    , Storable (Label dp)
    , Param_expr (Regression method expr dp)
    ) => Attributes dp -> Regression method expr dp -> Label dp
reg dp r = ((asRow $ dp2vec expr dp) LA.<> (beta r)) @@> (0,0)
    where
        expr = param_expr (error "poop3" :: Regression method expr dp)
