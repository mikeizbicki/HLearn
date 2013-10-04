import Test.Hspec

import qualified PerceptronSpec

main :: IO ()
main = hspec $ do 
  describe "Example test" $ do 
    it "always passes" $ do 
      1 `shouldBe` (1 :: Int)
  describe "Perceptron" PerceptronSpec.spec
