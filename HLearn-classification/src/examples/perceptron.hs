{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import HLearn.Algebra
import HLearn.Models.Classifiers

---------------------------------------

newtype Vector2 = Vector2 [Double]
    deriving (Read,Show,Eq)

instance Abelian Vector2
instance Monoid Vector2 where
    mempty = Vector2 [0,0]
    (Vector2 a) `mappend` (Vector2 b) = Vector2 $ zipWith (+) a b
    
instance Group Vector2 where
    inverse (Vector2 xs) = Vector2 $ map negate xs
    
instance HasRing Vector2 where
    type Ring Vector2 = Double

instance Module Vector2 where
    r .* (Vector2 xs) = Vector2 $ fmap (r*) xs

instance MetricSpace Vector2 where
    distance (Vector2 a) (Vector2 b) = sqrt . sum . map (^2) $ zipWith (-) a b
    
instance Triangle Vector2 Double where
    x <| (Vector2 xs) = Vector2 $ x:xs
    (Vector2 xs) |> x = Vector2 $ xs++[x]
    
---------------------------------------

ds = [ (True, Vector2 [1,1])
     , (False,Vector2 [2,2])
     ]
     
dp = Vector2 [0.5,0.5]
     
p = train ds :: Perceptron Bool Vector2

nn = train ds :: NaiveNN  [] Bool Vector2

boundary = map (map (letter . classify p)) [[Vector2 [x/10,y/10] | x<-[0..20]] | y<-[0..20]]
    where 
        letter True = 'T'
        letter False = 'F'
        
printboundary = sequence  $ map putStrLn boundary