{-# LANGUAGE QuasiQuotes,OverlappingInstances #-}

import Control.Monad
import Criterion.Types
import Criterion.Measurement

import HLearn.Algebra hiding (Product)
import HLearn.Evaluation.RSquared
import HLearn.Models.Classifiers.Common hiding (Regression(..))
import HLearn.Models.Regression
import HLearn.Models.Regression.Parsing

fib :: Double->Double
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

main = do
--     let runtimeL = [1.1,2.3,3.1,4.1,5.8,6.8]

    let inputL = concat $ replicate 1 [1..35]
    runtimeL <- sequence $ map (\x -> time_ $ run (nf fib x) 1) inputL
    let dataset = zip runtimeL inputL

    let model = train dataset :: Product
           '[ Regression (Just Linear) (Just [expr| 1+x |]) (Double,Double)
            , Regression (Just Linear) (Just [expr| 1+x^2 |]) (Double,Double)
            , Regression (Just Linear) (Just [expr| 1+x^3 |]) (Double,Double)
            , Regression (Just Linear) (Just [expr| 1+x^4 |]) (Double,Double)

            , Regression (Just Linear) (Just [expr| 1+log x |]) (Double,Double)
            , Regression (Just Linear) (Just [expr| 1+(log x)*x |]) (Double,Double)


            , Regression (Just Linear) (Just [expr| 1+2^x |]) (Double,Double)
            , Regression (Just Linear) (Just [expr| 1+(1/2)^x |]) (Double,Double)

            , Regression (Just Linear) (Just [expr| 1+x^x |]) (Double,Double)
            ] 

    

    sequence_ $ hlist2list $ hmap (RunTest dataset) $ unProduct model

    putStrLn "done"


type ProblemGen a = Int -> a

intMaker :: ProblemGen Int
intMaker = id

data RunTest = RunTest [(Double,Double)]
instance
    ( Floating (Label (Datapoint m))
    , Show (Label (Datapoint m))
    , Show m
    , NumDP m
    , Classifier m
    , Ring m ~ Label (Datapoint m)
    , Datapoint m ~ (Double,Double)
    ) => Function RunTest m (IO ()) 
        where
    function (RunTest xs) m = do
        let r = rsquared m xs
        let str = show m
        putStrLn $ str ++ replicate (50-length str) ' ' ++ "R^2 = " ++ show r

runtest m xs = do
    let r = rsquared m xs
    let str = show m
    putStrLn $ str ++ replicate (50 - length str) ' ' ++ "R^2 = " ++ show r

instance HasRing Double where
    type Ring Double = Double

-------------------------------------------------------------------------------

instance HomTrainer (HList '[]) where
    type Datapoint (HList '[]) = ()
    train1dp _ = HNil

instance 
    ( HomTrainer x
    , HomTrainer (HList xs)
    , HCons (Datapoint x) (Datapoint (HList xs)) ~ HList ( t ': ts)
    , t ~ Datapoint x
    , HList ts ~ Datapoint (HList xs)
    ) => HomTrainer (HList (x ': xs)) 
        where
    type Datapoint (HList (x ': xs)) = Datapoint x `HCons` Datapoint (HList xs)
    train1dp (x ::: xs) = train1dp x ::: train1dp xs 

---------------------------------------

newtype Product (xs :: [*]) = Product { unProduct :: HList xs }

deriving instance (Show (HList xs)) => Show (Product xs)

instance Monoid (Product '[]) where
    mempty = Product HNil
    mappend a b = Product HNil

instance (Monoid x, Monoid (HList xs)) => Monoid (Product (x ': xs)) where
    mempty = Product $ mempty:::mempty
    mappend (Product (a:::as)) (Product (b:::bs)) = Product $ a<>b ::: as<>bs

instance 
    ( HomTrainer (Product xs)
    , Monoid (HList xs)
    , HomTrainer x
    , Datapoint x ~ Datapoint (Product xs)
    ) => HomTrainer (Product (x ': xs))
        where
    type Datapoint (Product (x ': xs)) = Datapoint x
    train1dp x = Product $ train1dp x ::: (unProduct $ (train1dp x :: Product xs))

instance 
    ( HomTrainer x
    ) => HomTrainer (Product '[x]) where
    type Datapoint (Product '[x]) = Datapoint x
    train1dp dp = Product $ train1dp dp ::: HNil

instance HMap f (HList xs) (HList ys) => HMap f (Product xs) (Product ys) where
    hmap f (Product xs) = Product $ hmap f xs
