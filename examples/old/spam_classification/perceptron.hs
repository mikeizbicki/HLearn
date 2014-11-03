-- Very simple perceptron classifier to classify spam mails
-- Dataset from UCI Machine Learning Repository
-- Num of features - 57
-- Number of training examples - 4501
-- Number of test examples - 100

--------------------------------------------------
--                                             ---
-- usage :: ./perceptron train.data test.data  ---
--                                             ---
--------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 -optc-O2 #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import HLearn.Algebra
import HLearn.Models.Classifiers
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Split
import System.Environment

newtype DataPoint = DataPoint [Double]
    deriving (Read,Show,Eq,Ord)

instance Abelian DataPoint
instance Monoid DataPoint where
    mempty = DataPoint [0, 0]
    (DataPoint a) `mappend` (DataPoint b) = DataPoint $ zipWith (+) a b

instance Group DataPoint where
    inverse (DataPoint xs) = DataPoint $ map negate xs

instance HasRing DataPoint where
    type Ring DataPoint = Double

instance Module DataPoint where
    r .* (DataPoint xs) = DataPoint $ fmap (r*) xs

instance MetricSpace DataPoint where
    distance (DataPoint a) (DataPoint b) = sqrt . sum . map (^2) $ zipWith (-) a b

toDouble :: [String] -> [Double]
toDouble = map read

logNormalize :: Double -> Double
logNormalize x = log (x + 0.1)

parseText :: B.ByteString -> (Int, DataPoint)
parseText bstr = (label, features)
  where
    line = splitOn "," (B.unpack bstr)
    label = read (head line) :: Int
    -- features = DataPoint $ toDouble (tail line)
    features = DataPoint $ map logNormalize $ toDouble (tail line)

accuracy :: [(Int,Int)] -> Int
accuracy [] = 0
accuracy ((actual_label, expected_label):labels)
  | actual_label == expected_label = 1 + accuracy labels
  | otherwise = accuracy labels

classifyInstance :: (Perceptron Int DataPoint) -> (Int, DataPoint) -> (Int,Int)
classifyInstance m inst = (actual_label, predicted_label)
  where
    actual_label = fst inst
    predicted_label = classify m $ snd inst

main = do
  args <- getArgs

  let trainfile_name = head args
  let testfile_name = last args

  train_lines <- B.lines `fmap` B.readFile trainfile_name
  let instances = map parseText train_lines

  let m = train instances :: Perceptron Int DataPoint
  
  test_lines <- B.lines `fmap` B.readFile testfile_name
  let tests = map parseText test_lines

  let results = map (classifyInstance m) tests
  print $ (fromIntegral (accuracy results)) / (fromIntegral (length results))
