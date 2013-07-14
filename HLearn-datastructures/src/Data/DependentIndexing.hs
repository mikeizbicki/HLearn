module Data.DependentIndexing
--     ( 
--     Index, DependentIndex
-- 
--     -- ** dependent tuple indexing
--     , TI_0
--     , TI_1
--     , TI_2
--     , TI_3
--     )
    where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- classes

-- | index into a data type, all of whose members have the same type
class Index datatype index result | datatype -> index result where
    (!) :: datatype -> index -> result

-- | index into a data type, but the result type depends on which record we're indexing
class DependentIndex datatype index result | datatype index -> result where
    (#) :: datatype -> index -> result

-------------------------------------------------------------------------------
-- template haskell

type ConstructorFieldInfo = (Name,Strict,Type)

makeIndexData :: Name -> Q [Dec]
makeIndexData datatype = do
    xs <- extractFieldStrings datatype
    return $ [ DataD [] (mkName $ "Index_" ++ nameBase datatype) []
        (map (\x -> NormalC (mkName $ "Index_" ++ x) []) xs)
        []]

makeDependentIndexData :: Name -> Q [Dec]
makeDependentIndexData datatype = do
    xs <- extractFieldStrings datatype
    return $ map (\x -> DataD [] (mkName $ "TI_"++x) [] [NormalC (mkName $ "TI_"++x) []] [] ) xs

makeDependentIndexClass :: Name -> Q [Dec]
makeDependentIndexClass datatype = do
    xs <- extractConstructorFields datatype
    return $ map (\(x,_,t) -> InstanceD [] (AppT (AppT (AppT (ConT $ mkName "DependentIndex") ( ConT $ mkName "Funky")) (ConT $ mkName $ "TI_"++nameBase x)) (t)  ) 
        [ FunD (mkName "#") [ Clause [VarP $ mkName "datatype", VarP $ mkName "index"] (NormalB $ AppE (VarE x) (VarE $ mkName "datatype")) [] ] 
        ]) xs

-- makeIndexClass :: Name -> Q [Dec]
-- makeIndexClass datatype = do
    

extractFieldStrings :: Name -> Q [String]
extractFieldStrings datatype = do
    xs <- extractConstructorFields datatype
    return $ do 
        (name,_,_) <- xs
        return $ nameBase name

extractConstructorFields :: Name -> Q [ConstructorFieldInfo]
extractConstructorFields datatype = do
    let datatypeStr = nameBase datatype
    i <- reify datatype
    return $ case i of
        TyConI (DataD _ _ _ [RecC _ fs] _) -> fs
        TyConI (NewtypeD _ _ _ (RecC _ fs) _) -> fs
        TyConI (DataD _ _ _ [_] _) -> error $ "Can't derive Lens without record selectors: " ++ datatypeStr
        TyConI NewtypeD{} -> error $ "Can't derive Lens without record selectors: " ++ datatypeStr
        TyConI TySynD{} -> error $ "Can't derive Lens for type synonym: " ++ datatypeStr
        TyConI DataD{} -> error $ "Can't derive Lens for tagged union: " ++ datatypeStr
        _ -> error $ "Can't derive Lens for: " ++ datatypeStr ++ ", type name required."

-------------------------------------------------------------------------------
-- tupes

data TI_0 = TI_0
data TI_1 = TI_1
data TI_2 = TI_2
data TI_3 = TI_3
data TI_4 = TI_4
data TI_5 = TI_5
data TI_6 = TI_6
data TI_7 = TI_7
data TI_8 = TI_8
data TI_9 = TI_9

instance DependentIndex (a,b) TI_0 a where (a,b) # TI_0 = a
instance DependentIndex (a,b) TI_1 b where (a,b) # TI_1 = b

instance Index (a,a) Int a where
    (!) (a0,a1) 0 = a0
    (!) (a0,a1) 1 = a1

instance DependentIndex (a,b,c) TI_0 a where (a,b,c) # TI_0 = a
instance DependentIndex (a,b,c) TI_1 b where (a,b,c) # TI_1 = b
instance DependentIndex (a,b,c) TI_2 c where (a,b,c) # TI_2 = c

instance Index (a,a,a) Int a where
    (!) (a0,a1,a2) 0 = a0
    (!) (a0,a1,a2) 1 = a1
    (!) (a0,a1,a2) 2 = a2

instance DependentIndex (a,b,c,d) TI_0 a where (a,b,c,d) # TI_0 = a
instance DependentIndex (a,b,c,d) TI_1 b where (a,b,c,d) # TI_1 = b
instance DependentIndex (a,b,c,d) TI_2 c where (a,b,c,d) # TI_2 = c
instance DependentIndex (a,b,c,d) TI_3 d where (a,b,c,d) # TI_3 = d

instance Index (a,a,a,a) Int a where
    (!) (a0,a1,a2,a3) 0 = a0
    (!) (a0,a1,a2,a3) 1 = a1
    (!) (a0,a1,a2,a3) 2 = a2 
    (!) (a0,a1,a2,a3) 3 = a3

