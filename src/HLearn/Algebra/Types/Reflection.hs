{-# LANGUAGE DataKinds,PolyKinds,QuasiQuotes,RankNTypes,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances #-}

module HLearn.Algebra.Types.Reflection
    where

import Control.Monad
import Data.List hiding ((\\))
import Data.Maybe

import Data.Constraint
import Data.Constraint.Unsafe
import Data.Reflection
import Data.Proxy
import Debug.Trace

import Language.Haskell.TH hiding (reify)
import Language.Haskell.TH.Syntax hiding (reify)
import qualified Language.Haskell.TH as TH

import HLearn.Algebra

-------------------------------------------------------------------------------
-- types

-- data Param a = ParamUndef | RunTimeParam | SetParam a
-- :: Regression (Double,Double) [setparam| expr = 1 +x, method = linear |]
-- :: Regression (# Double,Double #) { expr = "1+x", method = Ridge { lambda = 0.4 } }

newtype ConstraintLift (p :: * -> Constraint) (a :: *) (s :: *) = ConstraintLift { lower :: a }

class ReifiableConstraint p where
    data Def (p :: * -> Constraint) (a:: *) :: *
    reifiedIns :: Reifies s (Def p a) :- p (ConstraintLift p a s)

class SetParam p m where
    data DefParam p m :: *
    setParam :: DefParam p m -> (p m => m) -> m

---------------------------------------

asProxyOf :: f s -> Proxy s -> f s
asProxyOf v _ = v

using' :: forall p a b. ReifiableConstraint p => Def p a -> (p a => a) -> (a -> b) -> b
using' d m f = reify d $ \(_ :: Proxy s) ->
    let replaceProof :: Reifies s (Def p a) :- p a
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p (ConstraintLift p a s) :- p a
    in (f m) \\ replaceProof

using :: forall p a. ReifiableConstraint p => Def p a -> (p a => a) -> a
using d m = reify d $ \(_ :: Proxy s) ->
    let replaceProof :: Reifies s (Def p a) :- p a
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p (ConstraintLift p a s) :- p a
    in m \\ replaceProof
    
using2 :: (ReifiableConstraint p1, ReifiableConstraint p2) => 
    (Def p1 a, Def p2 a) -> ((p1 a, p2 a) => a) -> a
using2 (p1,p2) f = using p1 $ using p2 $ f

setParam2 :: (SetParam p1 m, SetParam p2 m) => 
    (DefParam p1 m, DefParam p2 m) -> ((p1 m, p2 m) => m) -> m
setParam2 (p1,p2) f = setParam p1 $ setParam p2 $ f

-------------------------------------------------------------------------------
-- template haskell
 
mkParams :: Name -> Q [Dec]
mkParams dataName = do
    tmp <- TH.reify dataName
    let varL = case tmp of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _) -> xs

    let varL' = map mapgo $ filter filtergo varL
        filtergo (KindedTV _ (AppT (ConT maybe) _)) = nameBase maybe=="Maybe"
        filtergo _ = False
        mapgo (KindedTV name (AppT _ k)) = 
            (nameBase name,k,sing_kind2type k)


    paramClass <- liftM concat $ mapM (\(n,k,t) -> mkParamClass n $ return t) varL' 
    reifiableC <- liftM concat $ mapM (\(n,k,t) -> mkReifiableConstraint' 
            (mkName $ "Param_"++ n) 
            [SigD (mkName $ "param_"++n) $ AppT (AppT ArrowT (VarT $ mkName "m")) t ])
         varL' 
    paramInsts <- liftM concat $ mapM (\(n,k,t) -> mkParamInstance n t dataName) varL' 

    trace ("varL' = "++show varL') $ 
        return $ paramClass++reifiableC++paramInsts
--     trace ("varL' = "++show varL') $ return []

sing_kind2type :: Type -> Type
sing_kind2type (AppT ListT t) = AppT ListT $ sing_kind2type t
sing_kind2type (ConT n) = ConT $ mkName $ case nameBase n of
    "Nat" -> "Int"
    "TermT" -> "Term"
    "Method" -> "Method"
    otherwise -> error $ "nameBase n = " ++ nameBase n

param2class :: Name -> Name
param2class p = mkName $ "Param_" ++ nameBase p

param2func :: Name -> Name
param2func p = mkName $ "param_" ++ nameBase p

-- mkValidParams :: [String] -> Name -> Q [Dec]
-- mkValidParams paramStrL dataName = do
--     return 
--         [ ClassD
--             []
--             (mkName $ "AllParamsDefined_"++nameBase dataName)

mkParamInstance :: String -> Type -> Name -> Q [Dec]
mkParamInstance paramStr paramType dataName  = do
    c <- TH.reify dataName
    let tyVarL = case c of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            otherwise -> error $ "c = "++show c

    let tyVarL' = filter filtergo tyVarL
        filtergo (KindedTV n k) = nameBase n==paramStr
        filtergo (PlainTV n) = nameBase n == paramStr

    let [KindedTV paramName _] = tyVarL'

    return
        [ InstanceD
            [ ClassP
                (mkName "SingI")
                [ VarT paramName ]
            ]
            (AppT 
                (ConT $ param2class paramName)
                (tyVarL2Type tyVarL (AppT (PromotedT $ mkName "Just") (VarT paramName))))
            [ FunD
                (param2func paramName )
                [ Clause
                    [ VarP $ mkName "m" ]
                    (NormalB $
                        (AppE
                            (VarE $ mkName "fromSing")
                            (SigE
                                (VarE $ mkName "sing")
                                (AppT
                                    (ConT $ mkName "Sing")
                                    (VarT paramName)
                                )
                            )
                        )
                    )
                    []
                ]
            ]
        , InstanceD
            []
            (AppT 
                (AppT
                    (ConT $ mkName "SetParam")
                    (ConT (param2class paramName))
                )
                (tyVarL2Type tyVarL (PromotedT $ mkName "Nothing"))
            )
            [ DataInstD
                []
                (mkName $ "DefParam")
                [ ConT $ param2class paramName, tyVarL2Type tyVarL (PromotedT $ mkName "Nothing") ]
                [ RecC 
                    (mkName $ "SetParam_"++nameBase paramName)
                    [(mkName $ "unSetParam_"++nameBase paramName,NotStrict,paramType)]
                ]
                []
            , FunD
                (mkName $ "setParam")
                [ Clause
                    [VarP $ mkName "p", VarP $ mkName "a"]
                    (NormalB $
                        AppE
                            (AppE
                                (VarE $ mkName "using")
                                (AppE
                                    (ConE $ mkName $ "Def_Param_"++nameBase paramName)
                                    (LamE
                                        [VarP $ mkName"x"]
                                        (AppE
                                            (VarE $ mkName $ "unSetParam_"++nameBase paramName)
                                            (VarE $ mkName "p")
                                        )
                                    )
                                )
                            )
                            (VarE $ mkName "a")
                    )
                    []
                ]
            ]

        ]

    where
        tyVarL2Type xs matchType = go $ reverse xs
            where
                go [] = ConT $ mkName $ nameBase dataName
                go ((PlainTV n):xs) = AppT (go xs) (VarT n)
                go ((KindedTV n k):xs) = AppT (go xs) $ if nameBase n==paramStr
                    then matchType 
                    else (VarT n)

mkParamClass :: String -> Q Type -> Q [Dec]
mkParamClass str qparamT = do
    paramT <- qparamT
    isDef <- lookupTypeName $ "Param_"++str
    return $ case isDef of
        Just _ -> []
        Nothing -> 
            [ ClassD
                []
                (mkName $ "Param_"++str) 
                [PlainTV $ mkName "m"]
                []
                [ SigD
                    (mkName $ "param_"++str) 
                    (AppT
                        (AppT
                            ArrowT
                            (VarT $ mkName "m"))
                        paramT)
                ]
            ]

mkReifiableConstraint :: Name -> Q [Dec]
mkReifiableConstraint c = do
    info <- TH.reify c
    let funcL = case info of
            ClassI (ClassD _ _ _ _ xs) _ -> xs
            otherwise -> error "mkReifiableConstraint parameter must be a type class"
    mkReifiableConstraint' c funcL

mkReifiableConstraint' :: Name -> [Dec] -> Q [Dec] 
mkReifiableConstraint' c funcL = do
    return $
        [ InstanceD 
            []
            (AppT (ConT $ mkName "ReifiableConstraint") (ConT c))
            [ DataInstD 
                []
                (mkName "Def")
                [ ConT c, VarT tyVar]
                [ RecC 
                    (mkName $ "Def_"++nameBase c) 
                    [ (mkName $ nameBase fname ++ "_", NotStrict, insertTyVar (tyVar) ftype) 
                        | SigD fname ftype <- funcL
                    ]
                ]
                []
            , ValD 
                (VarP $ mkName "reifiedIns") 
                (NormalB $ 
                    (AppE 
                        (ConE $ mkName "Sub")
                        (ConE $ mkName "Dict"))
                ) 
                []
            ]
        , InstanceD
            [ ClassP 
                ( mkName "Reifies" )
                [ VarT $ mkName "s"
                , AppT
                    (AppT
                        (ConT $ mkName "Def")
                        (ConT c))
                    (VarT $ mkName "a")
                ]
            ]
            (AppT 
                (ConT c) 
                (AppT 
                    (AppT 
                        (AppT (ConT $ mkName "ConstraintLift") (ConT c))
                        (VarT tyVar))
                    (VarT $ mkName "s"))
            )
            [ FunD 
                fname 
                [ Clause
                    [ VarP $ mkName "a" ]
                    (NormalB $
                        AppE
                            (AppE
                                (VarE $ mkName $ nameBase fname++"_")
                                (AppE 
                                    (VarE (mkName "reflect"))
                                    (VarE (mkName "a"))))
                            (AppE
                                (VarE $ mkName "lower")
                                (VarE $ mkName "a"))
                    )
                    [] 
                ]
                | SigD fname ftype <- funcL
            ]
        ]
    where

        tyVar = mkName "a"

        insertTyVar :: Name -> Type -> Type
        insertTyVar name (ForallT xs cxt t) = ForallT [] [] (insertTyVar name t)
        insertTyVar name (AppT t1 t2) = AppT (insertTyVar name t1) (insertTyVar name t2)
        insertTyVar name (VarT _) = VarT name
        insertTyVar name ArrowT = ArrowT
        insertTyVar name a = a

-------------------------------------------------------------------------------
-- test

data ReflectionTest1 (a::Maybe Nat) = ReflectionTest1 Int 
    deriving (Read,Show,Eq,Ord)

instance (ParamA (ReflectionTest1 a)) => Monoid (ReflectionTest1 a) where
    mempty = ReflectionTest1 a 
        where
            a = paramA (undefined::ReflectionTest1 a)
    mappend a b = a

instance (ParamA (ReflectionTest1 a)) => HomTrainer (ReflectionTest1 a) where
    type Datapoint (ReflectionTest1 a) = ()
    train1dp dp = mempty


data ReflectionTest (a::Maybe Nat) (b::Maybe Nat) = ReflectionTest Int Int Int
    deriving (Read,Show,Eq,Ord)

instance (ParamA (ReflectionTest a b), ParamB (ReflectionTest a b)) => Monoid (ReflectionTest a b) where
    mempty = ReflectionTest a b $ a+b
        where
            a = paramA (undefined::ReflectionTest a b)
            b = paramB (undefined::ReflectionTest a b)
    mappend a b = a

instance (ParamA (ReflectionTest a b), ParamB (ReflectionTest a b)) => HomTrainer (ReflectionTest a b) where
    type Datapoint (ReflectionTest a b) = ()
    train1dp dp = mempty

---------------------------------------

class ParamA p where paramA :: p -> Int

instance ReifiableConstraint ParamA where
    data Def ParamA a = ParamA { paramA_ :: Int }
    reifiedIns = Sub Dict

instance Reifies s (Def ParamA a) => ParamA (ConstraintLift ParamA a s) where
    paramA a = paramA_ (reflect a)

class ParamB p where paramB :: p -> Int

instance ReifiableConstraint ParamB where
    data Def ParamB a = ParamB { paramB_ :: Int }
    reifiedIns = Sub Dict

instance Reifies s (Def ParamB a) => ParamB (ConstraintLift ParamB a s) where
    paramB a = paramB_ (reflect a)

mkTest :: (ParamA (ReflectionTest a b), ParamB (ReflectionTest a b)) => ReflectionTest a b
mkTest = train1dp ()

instance SingI a => ParamA (ReflectionTest1 (Just a)) where
    paramA _ = fromIntegral $ fromSing (sing :: Sing a)

instance SingI a => ParamA (ReflectionTest (Just a) b) where
    paramA _ = fromIntegral $ fromSing (sing :: Sing a)

instance SingI b => ParamB (ReflectionTest a (Just b)) where
    paramB _ = fromIntegral $ fromSing (sing :: Sing b)

class SetParamA m where
    a :: Int -> DefParam ParamA m

instance SetParamA (ReflectionTest1 Nothing) where
    a = DefParam_ParamA1 . ParamA

instance SetParamA (ReflectionTest Nothing b) where
    a = DefParam_ParamA . ParamA

a1 = DefParam_ParamA1 . ParamA
instance SetParam ParamA (ReflectionTest1 Nothing) where
    data DefParam ParamA (ReflectionTest1 Nothing) = 
            DefParam_ParamA1 { unDefParam1 :: Def ParamA (ReflectionTest1 Nothing) }
    setParam p a = using (unDefParam1 p) a

a2 = DefParam_ParamA . ParamA
instance SetParam ParamA (ReflectionTest Nothing b) where
    data DefParam ParamA (ReflectionTest Nothing b) = 
            DefParam_ParamA { unDefParam :: Def ParamA (ReflectionTest Nothing b) }
    setParam p a = using (unDefParam p) a

b = DefParam_ParamB . ParamB
instance SetParam ParamB (ReflectionTest a Nothing) where
    data DefParam ParamB (ReflectionTest a Nothing) = 
            DefParam_ParamB { unDefParamB :: Def ParamB (ReflectionTest a Nothing ) }
    setParam p a = using (unDefParamB p) a

-------------------------------------------------------------------------------
-- simple instances

instance ReifiableConstraint Eq where
    data Def Eq a = Eq { eq_ :: a -> a -> Bool }
    reifiedIns = Sub Dict

instance Reifies s (Def Eq a) => Eq (ConstraintLift Eq a s) where
    a == b = eq_ (reflect a) (lower a) (lower b)

instance ReifiableConstraint Ord where
    data Def Ord a = Ord { compare_ :: a -> a -> Ordering }
    reifiedIns = Sub Dict

instance Reifies s (Def Ord a) => Eq (ConstraintLift Ord a s) where
    a == b = isEq $ compare_ (reflect a) (lower a) (lower b)
        where
            isEq EQ = True
            isEq _ = False

instance Reifies s (Def Ord a) => Ord (ConstraintLift Ord a s) where
    compare a b = compare_ (reflect a) (lower a) (lower b)

instance ReifiableConstraint Monoid where
    data Def Monoid a = Monoid { mappend_ :: a -> a -> a, mempty_ :: a }
    reifiedIns = Sub Dict

instance Reifies s (Def Monoid a) => Monoid (ConstraintLift Monoid a s) where
    mappend a b = ConstraintLift $ mappend_ (reflect a) (lower a) (lower b) 
    mempty = a where a = ConstraintLift $ mempty_ (reflect a)

