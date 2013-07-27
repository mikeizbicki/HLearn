import System.Environment

code :: Int -> String -> String -> String
code i familyTest instanceTest = concat $ map (++"\n") $
    [ "{-# LANGUAGE TypeOperators,DataKinds,KindSignatures,TypeFamilies,PolyKinds,UndecidableInstances #-}"
    , "import GHC.TypeLits"

    , "data Nat1 = Zero | Succ Nat1"
    ]
    ++
    case head familyTest of
        'a' -> 
            [ "type family Replicate1 (n :: Nat1) (x::a) :: [a]"
            , "type instance Replicate1 Zero x = '[]"
            , "type instance Replicate1 (Succ n) x = x ': (Replicate1 n x)"
            ]
        'b' -> 
            [ "type family Replicate1 (n :: Nat1) (x::a) :: [a]"
            , "type instance Replicate1 n x = Replicate1' '[] n x "
            , "type family Replicate1' (acc::[a]) (n :: Nat1) (x::a)  :: [a]"
            , "type instance Replicate1' acc Zero x = acc"
            , "type instance Replicate1' acc (Succ n) x = Replicate1' (x ': acc) n x "
            ]
        'c' -> 
            [ "type family Replicate1 (n :: Nat1) (x::a) :: [a]"
            , "type instance Replicate1 n x = Replicate1' n x '[]"
            , "type family Replicate1' (n :: Nat1) (x::a) (acc::[a]) :: [a]"
            , "type instance Replicate1' Zero x acc = acc"
            , "type instance Replicate1' (Succ n) x acc = Replicate1' n x (x ': acc)"
            ]
    ++
    [ "class Class a where"
    , "    f :: a -> a"

    , "data Data (xs::a) = X | Y"
    , "    deriving (Read,Show)"
    
    , "main = print test1"
    ]
    ++
    case head instanceTest of
        'a' -> 
            [ "instance (xs ~ Replicate1 ("++mkNat1 i++") ()) => Class (Data xs) where"
            , "    f X = Y"
            , "    f Y = X"
            , "test1 = f (X :: Data ( Replicate1 ("++mkNat1 i++") () ))"
            ]
        'b' -> 
            [ "instance (xs ~ ("++mkList i++") ) => Class (Data xs) where"
            , "    f X = Y"
            , "    f Y = X"
            , "test1 = f (X :: Data ( Replicate1 ("++mkNat1 i++") () ))"
            ]
        'c' -> 
            [ "instance (xs ~ Replicate1 ("++mkNat1 i++") ()) => Class (Data xs) where"
            , "    f X = Y"
            , "    f Y = X"
            , "test1 = f (X :: Data ( ("++mkList i++") ))"
            ]
        'd' -> 
            [ "instance (xs ~ ("++mkList i++") ) => Class (Data xs) where"
            , "    f X = Y"
            , "    f Y = X"
            , "test1 = f (X :: Data ( ("++mkList i++") ))"
            ]

mkList :: Int -> String
mkList 0 = " '[] "
mkList i = " () ': " ++ mkList (i-1)

mkNat1 :: Int -> String
mkNat1 0 = " Zero "
mkNat1 i = " Succ ( " ++ mkNat1 (i-1) ++ ")"

main = do
    numstr : familyTest : instanceTest : xs <- getArgs
    let num = read numstr :: Int
    
    putStrLn $ code num familyTest instanceTest 

