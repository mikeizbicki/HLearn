{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import HLearn.Algebra
import HLearn.Models.Classifiers

---------------------------------------

newtype Vector2 = Vector2 [Double]
    deriving (Read,Show,Eq,Ord)

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
    
---------------------------------------

ds = [ ('o', Vector2 [-1,1.5])
     , ('x', Vector2 [2,2])
     , ('t', Vector2 [1,-1])
     ]
     
m = train ds :: Perceptron Char Vector2

-- | main creates an ascii plot of where the decision boundary for the perceptron is
main = sequence_ $ map putStrLn boundary

boundary = map (map go) [[(x/10,y/10) | x<-[-20..20]] | y<-(reverse [-20..20])]
    where
        go (x,y)
            | x == 0 && y == 0 = '+'
            | x == 0           = '|'
            | y == 0           = '-'
            | otherwise        = classify m $ Vector2 [x,y]
        
