{-# LANGUAGE TemplateHaskell #-}

module HLearn.Models.Distributions.Multivariate.Internal.TemplateHaskell
    where

import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Syntax


makeTypeLenses :: Name -> Q [Dec]
makeTypeLenses name = do
    datatypes <- makeDatatypes name
    indexNames <- makeIndexNames name
    trainableInstance <- makeTrainable name
    return $ datatypes ++ indexNames ++ trainableInstance

makeDatatypes :: Name -> Q [Dec]
makeDatatypes name = fmap (map makeEmptyData) $ extractContructorNames name
    
makeIndexNames :: Name -> Q [Dec]
makeIndexNames name = fmap (map makeIndexName . zip [0..]) $ extractContructorNames name
    
makeTrainable :: Name -> Q [Dec]
makeTrainable name = do
    hlistType <- extractHListType name
    hlistExp <- extractHListExp (mkName "var") name
    return $ [InstanceD [] (AppT (ConT (mkName "Trainable")) (ConT name)) 
        [ TySynInstD (mkName "GetHList") [ConT name] (AppT (ConT $ mkName "HList") hlistType)
        , FunD (mkName "getHList") [Clause [VarP $ mkName "var"] (NormalB hlistExp) []]
        ]]

makeEmptyData str = DataD [] (mkName $ nameTransform str) [] [NormalC (mkName $ nameTransform str) []] []

makeIndexName (i,str) = InstanceD [] (AppT (ConT $ mkName "IndexName") (ConT $ mkName $ nameTransform str)) 
    [ TySynInstD (mkName "IndexNameOf") [ConT $ mkName $ nameTransform str] (AppT (ConT $ mkName "Nat1Box") (typeNat i))
    ]
    where
        typeNat 0 = ConT $ mkName "Zero"
        typeNat n = AppT (ConT $ mkName "Succ") $ typeNat (n-1)

extractTypeInfo name = do
    i <- reify name
    return $ case i of
        TyConI (DataD    _ n ts _ _) -> (n, ts)
        TyConI (NewtypeD _ n ts _ _) -> (n, ts)
                  

nameTransform :: String -> String
nameTransform str = "TH_"++str

type ConstructorFieldInfo = (Name, Strict, Type)

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


-- extractLensTypeInfo :: Name -> Q LensTypeInfo
-- extractLensTypeInfo datatype = do
--   let datatypeStr = nameBase datatype
--   i <- reify datatype
--   return $ case i of
--     TyConI (DataD    _ n ts _ _) -> (n, ts)
--     TyConI (NewtypeD _ n ts _ _) -> (n, ts)