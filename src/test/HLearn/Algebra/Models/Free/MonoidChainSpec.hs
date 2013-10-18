{-# LANGUAGE DataKinds #-}

module HLearn.Algebra.Models.Free.MonoidChainSpec (spec) where
import HLearn.Algebra.Models.Free.MonoidChain

import Data.Monoid

import Test.QuickCheck
import Test.Hspec

import HLearn.Models.Distributions

spec :: Spec
spec = do
  describe "Algebra.Models.Free.MonoidChain" $ do
    it "is associative" $ property $ 
      testassociativity

testassociativity :: IO ()
testassociativity = quickCheck ((\m1 m2 m3 -> m1<>(m2<>m3)==(m1<>m2)<>m3) 
    :: MonoidChain 3 (Normal Rational Rational)
    -> MonoidChain 3 (Normal Rational Rational)
    -> MonoidChain 3 (Normal Rational Rational)
    -> Bool
    )
