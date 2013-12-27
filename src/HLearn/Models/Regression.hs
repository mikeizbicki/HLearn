{-# LANGUAGE DataKinds,PolyKinds #-}

module HLearn.Models.Reg
    where

import Data.List
import qualified Data.Foldable as F
import qualified Data.Semigroup as SG

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra hiding (Function(..))
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data types

data Reg eq dp = Reg 
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
    ) => HomTrainer (AddUnit1 (Reg eq) dp)
        where
    type Datapoint (AddUnit1 (Reg eq) dp) = dp
    
    train1dp dp = UnitLift1 $ mkReg
        (1)
        (scale (getLabel dp) (dp2vec $ getAttributes dp))
        ((asColumn $ dp2vec $ getAttributes dp) LA.<> (asRow $ dp2vec $ getAttributes dp))

-------------------------------------------------------------------------------
-- model use

-- reg :: Attributes dp -> Reg eq dp -> Label dp
reg dp r = (asRow $ dp2vec dp) LA.<> (beta r)



dp2vec :: (Num dp, Storable dp) => dp -> Vector dp
-- dp2vec dp = LA.fromList [1] 
dp2vec dp = LA.fromList [1,dp,dp*dp] 
-- dp2vec dp = LA.fromList [dp] 

-- class MkVec (e :: Expr) where
--     mkvec :: (Num dp, Storable dp) => Sing e -> dp -> Vector dp

-------------------------------------------------------------------------------
-- parsing

data Expr = Expr [Term]

data Term
    = Var 
    | MonC MonOp Term
    | BinC BinOp Term Term 
    deriving (Read,Show,Eq)

data MonOp = Log | Neg | Sin
    deriving (Read,Show,Eq)

data BinOp = Mult | Div
    deriving (Read,Show,Eq)

---------------------------------------

data instance Sing (f::Term) = STerm Term
data instance Sing (f::MonOp) = SMonOp MonOp 
data instance Sing (f::BinOp) = SBinOp BinOp

instance SingI Var where 
    sing = STerm Var
instance (SingI m, SingI t) => SingI (MonC m t) where 
    sing = STerm (MonC (fromSing (sing::Sing m)) (fromSing (sing::Sing t)))
instance (SingI m, SingI t1, SingI t2) => SingI (BinC m t1 t2) where
    sing = STerm (BinC (fromSing (sing::Sing m)) (fromSing (sing::Sing t1)) (fromSing (sing::Sing t2)))

instance SingI Log where sing = SMonOp Log 
instance SingI Sin where sing = SMonOp Sin
instance SingI Neg where sing = SMonOp Neg
instance SingI Mult where sing = SBinOp Mult
instance SingI Div where sing = SBinOp Div

instance SingE (Kind :: Term)  Term  where fromSing (STerm  f) = f
instance SingE (Kind :: MonOp) MonOp where fromSing (SMonOp f) = f
instance SingE (Kind :: BinOp) BinOp where fromSing (SBinOp f) = f

-- instance Floating r => SingE (Kind :: Term)  (r->r)    where fromSing (STerm f) = evalTerm f
-- instance Floating r => SingE (Kind :: MonOp) (r->r)    where fromSing (SMonOp f) = evalMonOp f 
-- instance Floating r => SingE (Kind :: BinOp) (r->r->r) where fromSing (SBinOp f) = evalBinOp f

evalTerm :: Floating x => Term -> x -> x
evalTerm Var x = x
evalTerm (MonC f t) x = (evalMonOp f)  (evalTerm t x) 
evalTerm (BinC f t1 t2) x = (evalBinOp f) (evalTerm t1 x) (evalTerm t2 x)

evalMonOp :: Floating x => MonOp -> x -> x
evalMonOp Log = log
evalMonOp Neg = negate
evalMonOp Sin = sin

evalBinOp :: Floating x => BinOp -> x -> x -> x
evalBinOp Mult = (*)
evalBinOp Div = (/)

-- train [] :: LinearRegression [expr| 1 + x + x^2 + log x ] Double
