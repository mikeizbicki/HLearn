-- Example.hs  --  Examples from HUnit user's guide
--
-- For more examples, check out the tests directory.  It contains unit tests
-- for HUnit. 

module Main where

import Test.HUnit


foo :: Int -> (Int, Int)
foo x = (1, x)

partA :: Int -> IO (Int, Int)
partA v = return (v+2, v+3)

partB :: Int -> IO Bool
partB v = return (v > 5)

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

test2 :: Test
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

tests' :: Test
tests' = test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? (foo 3),
                "test2" ~: do (x, y) <- partA 3
                              assertEqual "for the first result of partA," 5 x
                              partB y @? "(partB " ++ show y ++ ") failed" ]

main :: IO Counts
main = do runTestTT tests
          runTestTT tests'
