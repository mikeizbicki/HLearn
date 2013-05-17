{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides convenient TemplateHaskell functions for making type lens suitable for use with multivariate distributions.
-- 
-- Given a data type that looks like:
-- 
-- >data Character = Character
-- >    { _name      :: String
-- >    , _species   :: String
-- >    , _job       :: Job
-- >    , _isGood    :: Maybe Bool
-- >    , _age       :: Double -- in years
-- >    , _height    :: Double -- in feet
-- >    , _weight    :: Double -- in pounds
-- >    }
-- >    deriving (Read,Show,Eq,Ord)
-- > 
-- >data Job = Manager | Crew | Henchman | Other
-- >    deriving (Read,Show,Eq,Ord)
-- 
--
-- when we run the command:
--
-- >makeTypeLenses ''Character
--
-- We generate the following type lenses automatically:
--
-- >data TH_name    = TH_name
-- >data TH_species = TH_species
-- >data TH_job     = TH_job
-- >data TH_isGood  = TH_isGood
-- >data TH_age     = TH_age
-- >data TH_height  = TH_height
-- >data TH_weight  = TH_weight
-- >
-- >instance TypeLens TH_name where
-- >    type instance TypeLensIndex TH_name = Nat1Box Zero
-- >instance TypeLens TH_species where
-- >    type instance TypeLensIndex TH_species = Nat1Box (Succ Zero)
-- >instance TypeLens TH_job where
-- >    type instance TypeLensIndex TH_job = Nat1Box (Succ (Succ Zero))
-- >instance TypeLens TH_isGood where
-- >    type instance TypeLensIndex TH_isGood = Nat1Box (Succ (Succ (Succ Zero)))
-- >instance TypeLens TH_age where
-- >    type instance TypeLensIndex TH_age = Nat1Box (Succ (Succ (Succ (Succ Zero))))
-- >instance TypeLens TH_height where
-- >    type instance TypeLensIndex TH_height = Nat1Box (Succ (Succ (Succ (Succ (Succ Zero)))))
-- >instance TypeLens TH_weight where
-- >    type instance TypeLensIndex TH_weight = Nat1Box (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
-- >        
-- >instance Trainable Character where
-- >    type instance GetHList Character = HList '[String,String,Job,Maybe Bool, Double,Double,Double]
-- >    getHList var = name var:::species var:::job var:::isGood var:::age var:::height var:::weight var:::HNil
-- >
-- >instance MultivariateLabels Character where
-- >    getLabels dist = ["TH_name","TH_species","TH_job","TH_isGood","TH_age","TH_height","TH_weight"]
-- 
-- 
-- 

module HLearn.Models.Distributions.Multivariate.Internal.TypeLens
    ( 
    -- * Lens
    Trainable (..)
    , TypeLens (..)
    , TypeFunction (..)
    -- * TemplateHaskell
    , makeTypeLenses
    , nameTransform
    )
    where

import HLearn.Algebra
import Language.Haskell.TH hiding (Range)
import Language.Haskell.TH.Syntax hiding (Range)


-------------------------------------------------------------------------------
-- Trainable

-- | The Trainable class allows us to convert data types into an isomorphic "HList".  All of our multivariate distributions work on "HList"s, so they work on all instances of "Trainable" as well.
class Trainable t where
    type GetHList t
    getHList :: t -> GetHList t

instance Trainable (HList '[]) where
    type GetHList (HList '[]) = HList '[]
    getHList t = t
    
instance (Trainable (HList xs)) => Trainable (HList (x ': xs)) where
    type GetHList (HList (x ': xs)) = HList (x ': xs)
    getHList t = t

-- | This specifies a type level natural number (i.e. "Nat1") that indexes at the right location into our HList
class TypeLens i where
    type TypeLensIndex i

class TypeFunction f where
    type Domain f
    type Range f
    
    typefunc :: f -> Domain f -> Range f

-- | given the name of one of our records, transform it into the name of our type lens
nameTransform :: String -> String
nameTransform str = "TH"++str

nameTransform' :: Name -> Name
nameTransform' name = mkName $ "TH"++(nameBase name)

-- | constructs the type lens
makeTypeLenses :: Name -> Q [Dec]
makeTypeLenses name = do
    datatypes <- makeDatatypes name
    indexNames <- makeIndexNames name
    trainableInstance <- makeTrainable name
    multivariateLabels <- makeMultivariateLabels name
    typeFunctions <- makeTypeFunctions name
    return $ datatypes ++ indexNames ++ trainableInstance ++ multivariateLabels ++ typeFunctions

makeDatatypes :: Name -> Q [Dec]
makeDatatypes name = fmap (map makeEmptyData) $ extractContructorNames name
    where
        makeEmptyData str = DataD [] (mkName $ nameTransform str) [] [NormalC (mkName $ nameTransform str) []] []

makeIndexNames :: Name -> Q [Dec]
makeIndexNames name = fmap (map makeIndexName . zip [0..]) $ extractContructorNames name
    where
    makeIndexName (i,str) = InstanceD [] (AppT (ConT $ mkName "TypeLens") (ConT $ mkName $ nameTransform str)) 
        [ TySynInstD (mkName "TypeLensIndex") [ConT $ mkName $ nameTransform str] (AppT (ConT $ mkName "Nat1Box") (typeNat i))
        ]
        where
            typeNat 0 = ConT $ mkName "Zero"
            typeNat n = AppT (ConT $ mkName "Succ") $ typeNat (n-1)

makeTypeFunctions :: Name -> Q [Dec]
makeTypeFunctions constructorName = fmap (map makeTypeFunction) $ extractConstructorFields constructorName
    where
        makeTypeFunction (recordName,_,recordType) = InstanceD [] (AppT (ConT $ mkName "TypeFunction") (ConT $ nameTransform' recordName)) 
            [ TySynInstD (mkName "Domain") [ConT $ nameTransform' recordName] (ConT constructorName)
            , TySynInstD (mkName "Range") [ConT $ nameTransform' recordName] (SigT recordType StarT)
            , FunD (mkName "typefunc") [Clause [VarP $ mkName "_"{-, VarP $ mkName "domain"-}] (NormalB $ VarE recordName) []]
            ]

makeTrainable :: Name -> Q [Dec]
makeTrainable name = do
    hlistType <- extractHListType name
    hlistExp <- extractHListExp (mkName "var") name
    return $ [InstanceD [] (AppT (ConT (mkName "Trainable")) (ConT name)) 
        [ TySynInstD (mkName "GetHList") [ConT name] (AppT (ConT $ mkName "HList") hlistType)
        , FunD (mkName "getHList") [Clause [VarP $ mkName "var"] (NormalB hlistExp) []]
        ]]

makeMultivariateLabels :: Name -> Q [Dec]
makeMultivariateLabels name = do
    labelL <- extractContructorNames name
    return $ [ InstanceD [] (AppT (ConT (mkName "MultivariateLabels")) (ConT name)) 
        [ FunD (mkName "getLabels") [Clause [VarP $ mkName "dist"] (NormalB $ go labelL ) []]
        ]]
        where
            go [] = ConE $ mkName "[]"
            go (x:xs) = AppE (AppE (ConE $ mkName ":") (LitE $ StringL (nameTransform x))) $ go xs

---------------------------------------

extractHListType :: Name -> Q Type
extractHListType name = do
    typeL <- fmap (map getType) $ extractConstructorFields name
    return $ go typeL
    where
        go [] = ConT $ mkName "[]"
        go (x:xs) = AppT (AppT (ConT $ mkName ":") (x)) $ go xs
            
        getType (n,s,t) = t

extractHListExp :: Name -> Name -> Q Exp
extractHListExp var name = do
    typeL <- fmap (map getName) $ extractConstructorFields name
    return $ go typeL
    where
        go [] = ConE $ mkName "HNil"
        go (x:xs) = AppE (AppE (ConE $ mkName ":::") (AppE (VarE x) (VarE var))) $ go xs
            
        getName (n,s,t) = n

-------------------------------------------------------------------------------
-- below taken from Data.Lens 

type ConstructorFieldInfo = (Name, Strict, Type)

extractContructorNames :: Name -> Q [String]
extractContructorNames datatype = fmap (map name) $ extractConstructorFields datatype
    where
        name (n,s,t) = nameBase n

extractConstructorFields :: Name -> Q [ConstructorFieldInfo]
extractConstructorFields datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ _ _ [RecC _ fs] _) -> fs
    TyConI (NewtypeD _ _ _ (RecC _ fs) _) -> fs
    TyConI (DataD    _ _ _ [_]         _) -> error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI NewtypeD{} -> error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI TySynD{}   -> error $ "Can't derive Lens for type synonym: " ++ datatypeStr
    TyConI DataD{}    -> error $ "Can't derive Lens for tagged union: " ++ datatypeStr
    _                 -> error $ "Can't derive Lens for: "  ++ datatypeStr ++ ", type name required."

extractTypeInfo name = do
    i <- reify name
    return $ case i of
        TyConI (DataD    _ n ts _ _) -> (n, ts)
        TyConI (NewtypeD _ n ts _ _) -> (n, ts)
