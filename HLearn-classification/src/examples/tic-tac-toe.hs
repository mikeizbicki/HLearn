{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Data.Array as A
import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

data Player = X | O
    deriving (Read,Show,Eq,Ord)

data Square = Empty | Taken Player
    deriving (Read,Show,Eq,Ord)

newtype TTT = TTT 
    { board :: A.Array (Int,Int) Square
    }
    deriving (Read,Show,Eq,Ord)

newgame :: TTT
newgame = TTT $ A.listArray ((0,0),(2,2)) $ repeat Empty

move :: TTT -> (Int,Int) -> Player -> TTT
move ttt pos player = TTT $ board ttt A.// [(pos,Taken player)]

winner :: TTT -> Maybe Player
winner (TTT board) = if didOwin
    then Just O
    else if didXwin
            then Just X
            else Nothing
    where
        didXwin = or $ map (all (==Taken X)) posL
        didOwin = or $ map (all (==Taken O)) posL
        posL = map (map (board A.!)) positionL
        positionL = [ [(i,j)   | i<-[0,1,2]] | j<-[0,1,2] ]
                  ++[ [(j,i)   | i<-[0,1,2]] | j<-[0,1,2] ]
                  ++[ [(i,i)   | i<-[0,1,2]] ]
                  ++[ [(i,2-i) | i<-[0,1,2]] ]
    
-------------------------------------------------------------------------------
-- learning

-- instance Morphism TTT (NoParams TTT) (HVector ShowBox [Square,Square,Square,Square,Square,Square,Square,Square,Square])
--     where
--     
--     morph' ttt _ = vec undefined $ list2hlist $ A.elems $ board ttt
-- 
-- -- instance Morphism TTT (NoParams TTT) (HVector ShowBox (Replicate Square 9))
-- 
-- data VarType = Discrete | Continuous
-- 
-- -- class 
-- 
-- -- class HVector2Multivariate
-- 
-- -------------------------------------------------------------------------------
-- -- junk
-- 
