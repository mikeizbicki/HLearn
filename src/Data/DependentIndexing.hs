module Data.DependentIndexing
    ( 
    Index(..)
    , DependentIndex(..)

    -- ** template haskell
    , makeIndex

    -- ** dependent tuple indexing
    , DI_0
    , DI_1
    , DI_2
    , DI_3
    )
    where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- classes

-- | index into a data type, all of whose members have the same type
class 
    ( Eq (IndexType datatype)
    , Bounded (IndexType datatype)
    , Enum (IndexType datatype)
--     , Ord (IndexResult datatype)
    ) => Index datatype 
        where
    type IndexType datatype
    type IndexResult datatype
    (!) :: datatype -> IndexType datatype -> IndexResult datatype 
-- class Index datatype index result | datatype -> index result where
--     (!) :: datatype -> index -> result

-- | index into a data type, but the result type depends on which record we're indexing
class DependentIndex datatype index where
    type DependentIndexResult datatype index
    (#) :: datatype -> index -> DependentIndexResult datatype index 
-- class DependentIndex datatype index result | datatype index -> result where
--     (#) :: datatype -> index -> result

-------------------------------------------------------------------------------
-- template haskell

-- | Creates data types and instances for indexing into a class.  Dependent indices are preficed with \"DI_\" and normal indices by \"I_\".  The dependent indices and "DependentIndex" instances are always created.  The normal index data types are always created, but the "Index" instance is created only if all records within the data type have the same type.
makeIndex :: Name -> Q [Dec]
makeIndex datatype = do
    d2 <- makeDependentIndexData datatype
    c2 <- makeDependentIndexClass datatype
    d1 <- makeIndexData datatype

    xs <- extractConstructorFields datatype
    let (_,_,t0) = head xs
    c1 <- if fst $ foldr (\(_,_,t) (bool,t') -> (t==t' && bool,t)) (True,t0) xs
        then makeIndexClass datatype
        else return []
    return $ d1++c1++d2++c2

makeIndexData :: Name -> Q [Dec]
makeIndexData datatype = do
    xs <- extractFieldStrings datatype
    return $ [ 
        DataD 
            [] 
            (mkName $ "I_" ++ nameBase datatype) 
            []
            (map (\x -> NormalC (mkName $ "I_" ++ x) []) xs)
            [mkName "Show",mkName "Read",mkName "Eq",mkName "Ord",mkName "Bounded",mkName "Enum"] 
        ]

makeIndexClass :: Name -> Q [Dec]
makeIndexClass datatype = do
    xs <- extractConstructorFields datatype
    let (_,_,t) = head xs
    return $ [ 
        InstanceD 
            [] 
--             (AppT (AppT (AppT 
--                 (ConT $ mkName "Index") 
--                 (ConT datatype)) 
--                 (ConT $ mkName $ "I_"++nameBase datatype)) 
--                 (t)
--                 ) 
            (AppT (ConT $ mkName "Index") (ConT datatype)) 
            [ TySynInstD (mkName "IndexType"  ) [ConT datatype] (ConT $ mkName $ "I_"++nameBase datatype)
            , TySynInstD (mkName "IndexResult") [ConT datatype] t
            , FunD 
                (mkName "!") 
                (map (\(x,_,_) -> Clause 
                    [VarP $ mkName "datatype", ConP (mkName $ "I_"++nameBase x) []] 
                    (NormalB $ AppE (VarE x) (VarE $ mkName "datatype")) 
                    [] 
                    ) xs)
            ]
        ] 

makeDependentIndexData :: Name -> Q [Dec]
makeDependentIndexData datatype = do
    xs <- extractFieldStrings datatype
    return $ map (\x -> 
        DataD 
            [] 
            (mkName $ "DI_"++x) 
            [] 
            [NormalC (mkName $ "DI_"++x) []] 
            [mkName "Show", mkName "Read", mkName "Eq", mkName "Ord"] 
        ) xs

makeDependentIndexClass :: Name -> Q [Dec]
makeDependentIndexClass datatype = do
    xs <- extractConstructorFields datatype
    return $ map (\(x,_,t) -> 
        InstanceD 
            [] 
--             (AppT (AppT (AppT 
--                 (ConT $ mkName "DependentIndex") 
--                 (ConT datatype)) 
--                 (ConT $ mkName $ "DI_"++nameBase x)) (t)
--                 ) 
            (AppT (AppT (ConT $ mkName "DependentIndex") (ConT datatype)) (ConT $ mkName $ "DI_"++nameBase x))
            [ TySynInstD (mkName "DependentIndexResult") [ConT datatype, ConT $ mkName $ "DI_"++nameBase x] t
            , FunD 
                (mkName "#") 
                [ Clause 
                    [VarP $ mkName "datatype", VarP $ mkName "index"] 
                    (NormalB $ AppE (VarE x) (VarE $ mkName "datatype")) 
                    [] 
                ]
            ]
        ) xs

extractFieldStrings :: Name -> Q [String]
extractFieldStrings datatype = do
    xs <- extractConstructorFields datatype
    return $ do 
        (name,_,_) <- xs
        return $ nameBase name

type ConstructorFieldInfo = (Name,Strict,Type)

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

data DI_0 = DI_0
data DI_1 = DI_1
data DI_2 = DI_2
data DI_3 = DI_3
data DI_4 = DI_4
data DI_5 = DI_5
data DI_6 = DI_6
data DI_7 = DI_7
data DI_8 = DI_8
data DI_9 = DI_9

instance DependentIndex (a,b) DI_0 where 
    type DependentIndexResult (a,b) DI_0 = a
    (a,b) # DI_0 = a
instance DependentIndex (a,b) DI_1 where 
    type DependentIndexResult (a,b) DI_1 = b
    (a,b) # DI_1 = b

instance (Ord a, Num a) => Index (a,a) where
    type IndexType (a,a) = Int
    type IndexResult (a,a) = a
    (!) (a0,a1) 0 = a0
    (!) (a0,a1) 1 = a1

instance DependentIndex (a,b,c) DI_0 where 
    type DependentIndexResult (a,b,c) DI_0 = a
    (a,b,c) # DI_0 = a
instance DependentIndex (a,b,c) DI_1 where 
    type DependentIndexResult (a,b,c) DI_1 = b
    (a,b,c) # DI_1 = b
instance DependentIndex (a,b,c) DI_2 where 
    type DependentIndexResult (a,b,c) DI_2 = c
    (a,b,c) # DI_2 = c

instance (Ord a, Num a) =>  Index (a,a,a) where
    type IndexType (a,a,a) = Int
    type IndexResult (a,a,a) = a
    (!) (a0,a1,a2) 0 = a0
    (!) (a0,a1,a2) 1 = a1
    (!) (a0,a1,a2) 2 = a2

-- instance DependentIndex (a,b,c,d) DI_0 a where (a,b,c,d) # DI_0 = a
-- instance DependentIndex (a,b,c,d) DI_1 b where (a,b,c,d) # DI_1 = b
-- instance DependentIndex (a,b,c,d) DI_2 c where (a,b,c,d) # DI_2 = c
-- instance DependentIndex (a,b,c,d) DI_3 d where (a,b,c,d) # DI_3 = d
-- 
-- instance Index (a,a,a,a) Int a where
--     (!) (a0,a1,a2,a3) 0 = a0
--     (!) (a0,a1,a2,a3) 1 = a1
--     (!) (a0,a1,a2,a3) 2 = a2 
--     (!) (a0,a1,a2,a3) 3 = a3
-- 
