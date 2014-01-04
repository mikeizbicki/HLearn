{-# LANGUAGE QuasiQuotes #-}
import HLearn.Algebra hiding (Product)
import HLearn.Evaluation.RSquared
import HLearn.Models.Regression
import HLearn.Models.Regression.Parsing

main = do
    let runtimeL = zip [1.1,2.3,3.1,4.1,5.8,6.8] [0..]

    let m_linear = train runtimeL :: Regression (Just Linear) (Just [expr| 1 + x |]) (Double,Double)
    let m_quadratic = train runtimeL :: Regression (Just Linear) (Just [expr| 1 + x + x^2 |]) (Double,Double)
    let m_exp = train runtimeL :: Regression (Just Linear) (Just [expr| 1 + 2^x |]) (Double,Double)

    runtest m_linear runtimeL
    runtest m_quadratic runtimeL
    runtest m_exp runtimeL

    putStrLn "done"

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

instance Monoid (Product '[]) where
    mempty = Product HNil
    mappend a b = Product HNil

instance (Monoid x, Monoid (HList xs)) => Monoid (Product (x ': xs)) where
    mempty = Product $ mempty:::mempty
    mappend (Product (a:::as)) (Product (b:::bs)) = Product $ a<>b ::: as<>bs

instance 
    ( Monoid x
    , Monoid (HList xs)
    )  => HomTrainer (Product (x ': xs)) 
        where
    type Datapoint (Product (x ': xs)) = Datapoint x
    train1dp x = Product $ undefined -- hmap (\_ -> train1dp x) $ mempty

class HReplicate1 n x xs | n x -> xs where
    hreplicate :: (Nat1Box n) -> x -> HList xs

instance HReplicate1 Zero x '[] where
    hreplicate _ _ = HNil

instance HReplicate1 n x xs => HReplicate1 (Succ n) x (x ': xs) where
    hreplicate _ x = x:::hreplicate (undefined::Nat1Box n) x
