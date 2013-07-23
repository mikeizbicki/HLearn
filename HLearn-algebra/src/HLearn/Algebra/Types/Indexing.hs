module HLearn.Algebra.Types.Indexing
    ( 
    Index(..)
    , DepIndex(..)
    , HasDepIndex(..)
    , ValueList

    -- ** template haskell
    , makeIndex

    -- ** dependent tuple indexing
    , TH_0
    , TH_1
    , TH_2
    , TH_3
    )
    where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import HLearn.Algebra.Types.HList

-------------------------------------------------------------------------------
-- classes

-- | index into a data type, all of whose members have the same type
class 
    ( Eq (IndexType datatype)
    , Bounded (IndexType datatype)
    , Enum (IndexType datatype)
    ) => Index datatype 
        where
    type IndexType datatype
    type IndexResult datatype
    (!) :: datatype -> IndexType datatype -> IndexResult datatype 

-- | index into a data type, but the result type depends on which record we're indexing
class DepIndex datatype index where
    type DepIndexResult datatype index 
    (#) :: datatype -> index -> datatype `DepIndexResult` index 

type family (#) datatype index
type instance (#) datatype index = DepIndexResult datatype index

instance DepIndex (HList '[]) (Nat1Box Zero) where
    type DepIndexResult (HList '[]) (Nat1Box Zero) = ()
    _ # _ = ()

instance DepIndex (HList (x ': xs)) (Nat1Box Zero) where
    type DepIndexResult (HList (x ': xs)) (Nat1Box Zero) = x
    (x:::xs) # _ = x

instance (DepIndex (HList xs) (Nat1Box n)) => DepIndex (HList (x ': xs)) (Nat1Box (Succ n)) where
    type DepIndexResult (HList (x ': xs)) (Nat1Box (Succ n)) = DepIndexResult (HList xs) (Nat1Box n)
    (x:::xs) # Nat1Box = xs # (Nat1Box :: Nat1Box n)

-- | defines a canonical ordering of all elments in the data type
class (IndexList datatype (HList (DepIndexList datatype))) => HasDepIndex datatype where
    type DepIndexList datatype :: [*]
    depIndexList :: datatype -> HList (DepIndexList datatype)

    datatype2valueList :: 
        ( HasDepIndex datatype
        ) => datatype -> HList (ValueList datatype)
    datatype2valueList dp = valueList dp (depIndexList dp)

type ValueList datatype = IndexList2ValueList datatype (HList (DepIndexList datatype))

-- | this class exists only to ensure that all elements in the "DepIndexList" are actually valid "DepIndex"es
class IndexList dp indexL where
    type IndexList2ValueList dp indexL :: [*] 
    valueList :: dp -> indexL -> HList (IndexList2ValueList dp indexL)

instance IndexList dp (HList '[]) where
    type IndexList2ValueList dp (HList '[]) = '[]
    {-`DepIndexResult` INLINE valueList `DepIndexResult`-}
    valueList dp _ = HNil

instance 
    ( IndexList dp (HList xs)
    , DepIndex dp x
    ) => IndexList dp (HList (x ': xs)) 
        where
    type IndexList2ValueList dp (HList (x ': xs)) = (dp `DepIndexResult` x) ': (IndexList2ValueList dp (HList xs))
    {-`DepIndexResult` INLINE valueList `DepIndexResult`-}
    valueList dp (x:::xs) = dp # x ::: valueList dp xs


-------------------------------------------------------------------------------
-- template haskell

-- | Creates data types and instances for indexing into a class.  Dependent indices are preficed with \"TH_\" and normal indices by \"I_\".  The dependent indices and "DepIndex" instances are always created.  The normal index data types are always created, but the "Index" instance is created only if all records within the data type have the same type.
makeIndex :: Name -> Q [Dec]
makeIndex datatype = do
    d2 <- makeDepIndexData datatype
    c2 <- makeDepIndexClass datatype
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

depIndexPrefix :: String
depIndexPrefix = "TH"

makeDepIndexData :: Name -> Q [Dec]
makeDepIndexData datatype = do
    xs <- extractFieldStrings datatype
    return $ map (\x -> 
        DataD 
            [] 
            (mkName $ depIndexPrefix++x) 
            [] 
            [NormalC (mkName $ depIndexPrefix++x) []] 
            [mkName "Show", mkName "Read", mkName "Eq", mkName "Ord"] 
        ) xs

makeDepIndexClass :: Name -> Q [Dec]
makeDepIndexClass datatype = do
    xs <- extractConstructorFields datatype
    return $ map (\(x,_,t) -> 
        InstanceD 
            [] 
--             (AppT (AppT (AppT 
--                 (ConT $ mkName "DepIndex") 
--                 (ConT datatype)) 
--                 (ConT $ mkName $ depIndexPrefix++nameBase x)) (t)
--                 ) 
            (AppT (AppT (ConT $ mkName "DepIndex") (ConT datatype)) (ConT $ mkName $ depIndexPrefix++nameBase x))
            [ TySynInstD (mkName "DepIndexResult") [ConT datatype, ConT $ mkName $ depIndexPrefix++nameBase x] t
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
-- tuples

data TH_0 = TH_0
data TH_1 = TH_1
data TH_2 = TH_2
data TH_3 = TH_3
data TH_4 = TH_4
data TH_5 = TH_5
data TH_6 = TH_6
data TH_7 = TH_7
data TH_8 = TH_8
data TH_9 = TH_9

instance DepIndex (a,b) TH_0 where 
    type (a,b) `DepIndexResult` TH_0 = a
    (a,b) # TH_0 = a
instance DepIndex (a,b) TH_1 where 
    type (a,b) `DepIndexResult` TH_1 = b
    (a,b) # TH_1 = b

instance (Ord a, Num a) => Index (a,a) where
    type IndexType (a,a) = Int
    type IndexResult (a,a) = a
    (!) (a0,a1) 0 = a0
    (!) (a0,a1) 1 = a1

instance DepIndex (a,b,c) TH_0 where 
    type (a,b,c) `DepIndexResult` TH_0 = a
    (a,b,c) # TH_0 = a
instance DepIndex (a,b,c) TH_1 where 
    type (a,b,c) `DepIndexResult` TH_1 = b
    (a,b,c) # TH_1 = b
instance DepIndex (a,b,c) TH_2 where 
    type (a,b,c) `DepIndexResult` TH_2 = c
    (a,b,c) # TH_2 = c

instance (Ord a, Num a) =>  Index (a,a,a) where
    type IndexType (a,a,a) = Int
    type IndexResult (a,a,a) = a
    (!) (a0,a1,a2) 0 = a0
    (!) (a0,a1,a2) 1 = a1
    (!) (a0,a1,a2) 2 = a2

