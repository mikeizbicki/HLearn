{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HLearn.Models.Classifiers.PerceptronSpec (main, spec) where

import HLearn.Algebra
import HLearn.Models.Classifiers
import Test.Hspec

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
    distance (Vector2 a) (Vector2 b) = sqrt . sum . map (^(2 :: Integer)) $ zipWith (-) a b
    
---------------------------------------

ds :: [(Char, Vector2)]
ds = [ ('o', Vector2 [-1,1.5])
     , ('x', Vector2 [2,2])
     , ('t', Vector2 [1,-1])
     ]
     
m :: Perceptron Char Vector2
m = train ds 

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "exact given values" $ do 
    it "matches the value o" $ do 
      (classify m $ Vector2 [-1, 1.5]) `shouldBe` 'o'
    it "matches the value x" $ do 
      (classify m $ Vector2 [2, 2]) `shouldBe` 'x'
    it "matches the value t" $ do 
      (classify m $ Vector2 [1, -1]) `shouldBe` 't'

  describe "boundary values" $ do 
    it "matches the o region" $ do 
      (classify m $ Vector2 [0.6, 0.8]) `shouldBe` 'o'
    it "matches the x region" $ do
      (classify m $ Vector2 [0.7, 0.8]) `shouldBe` 'x'
    it "matches the t region" $ do
      (classify m $ Vector2 [0.6, 0.7]) `shouldBe` 't'
