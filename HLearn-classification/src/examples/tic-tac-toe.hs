{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Data.Array as A
import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

data Square = X | O | Empty
    deriving (Read,Show,Eq,Ord)

data TTT = TTT 
    { board :: A.Array (Int,Int) Square
    }
    deriving (Read,Show,Eq,Ord)

newgame :: TTT
newgame = TTT $ A.listArray ((0,0),(2,2)) $ repeat Empty

move :: TTT -> (Int,Int) -> Square -> TTT
move ttt pos square = TTT $ board ttt A.// [(pos,square)]

winner :: TTT -> Square
winner (TTT board) = if didOwin
    then O
    else if didOwin
            then O
            else Empty
    where
        didXwin = or $ map (all (==X)) posL
        didOwin = or $ map (all (==O)) posL
        posL = map (map (board A.!)) positionL
        positionL = [ [(i,j)   | i<-[0,1,2]] | j<-[0,1,2] ]
                  ++[ [(j,i)   | i<-[0,1,2]] | j<-[0,1,2] ]
                  ++[ [(i,i)   | i<-[0,1,2]] ]
                  ++[ [(i,2-i) | i<-[0,1,2]] ]
    
-------------------------------------------------------------------------------
-- learning

instance Morphism TTT (NoParams TTT) (HVector ShowBox [Square,Square,Square,Square,Square,Square,Square,Square,Square])
    where
    
    morph' ttt _ = vec undefined $ list2hlist $ A.elems $ board ttt

-- instance Morphism TTT (NoParams TTT) (HVector ShowBox (Replicate Square 9))

data VarType = Discrete | Continuous

-- class 

-- class HVector2Multivariate

-------------------------------------------------------------------------------
-- junk

