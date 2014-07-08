This program performs fast cross-validation on the Census Income data set in CSV format.
It's main purpose is for run time comparison with Weka.
 
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE BangPatterns #-}
> {-# LANGUAGE TemplateHaskell #-}
> 
> import Control.Applicative
> import Control.Monad
> import Control.Monad.Random
> import Data.Csv
> import qualified Data.Vector as V
> import qualified Data.ByteString.Lazy.Char8  as BS
> import System.Environment
> 
> import HLearn.Algebra
> import HLearn.Models.Distributions
> import HLearn.Models.Classifiers.Bayes
> import HLearn.Models.Classifiers.Common
> import HLearn.Evaluation.CrossValidation

We represent a data point with the data type Person.
The label is _income, and everything else is the attributes.
Sometimes, we will want to have a Person that has no label, but we will never want a Person with no attributes.
That's why _income is not strict, but everything else is.

> data Person = Person
>    { _income        :: String
>    , _workclass     :: !String
>    , _education     :: !String
>    , _maritalStatus :: !String
>    , _occupation    :: !String
>    , _relationship  :: !String
>    , _race          :: !String
>    , _sex           :: !String
>    , _nativeCountry :: !String
>    , _age           :: !Double
>    , _fnlwgt        :: !Double
>    , _educationNum  :: !Double
>    , _capitalGain   :: !Double
>    , _capitalLoss   :: !Double
>    , _hoursPerWeek  :: !Double
>    }
>    deriving (Read,Show,Eq,Ord)
> makeTypeLenses ''Person
> 
> data TestData = TestData { _a :: String, _b :: Int, _c :: Double }
> makeTypeLenses ''TestData
> instance Labeled TestData where
>    type Label TestData = String
>    type Attributes TestData = TestData
>    getLabel = _a
>    getAttributes = id
> testxs = [] :: [TestData]
> testm = train testxs :: Bayes TH_a TestDist
> testd = train testxs :: TestDist
> type TestDist = Multivariate TestData '[ MultiCategorical '[String], Independent Categorical '[Int], Independent Normal '[Double]] Double

All data points for supervised learning must implement the LabeledAttributes class.
Things are in a relatively "hacked together" state right now, and in the future this should become much cleaner.

> instance Labeled Person where
>    type Label Person = String
>    type Attributes Person = Person
>    getLabel = _income
>    getAttributes p = p

This is how we convert from the CSV format into our Person data type.
Person and the CSV do not have fields in the same order.
The ordering of Person's fields makes defining the Naive Bayes distribution type easier.

> instance FromRecord Person where
>    parseRecord v
>        | V.length v >= 14 = Person <$> 
>                            v .! 14 <*>
>                            v .! 1 <*>
>                            v .! 3 <*>
>                            v .! 5 <*>
>                            v .! 6 <*>
>                            v .! 7 <*>
>                            v .! 8 <*>
>                            v .! 9 <*>
>                            v .! 13<*>
>                            v .! 0 <*>
>                            v .! 2 <*>
>                            v .! 4 <*>
>                            v .! 10<*>
>                            v .! 11<*>
>                            v .! 12
>        | otherwise = error "mzero"

This is the type for our Bayesian classification.
It says that everything is independent of everything else, except for income which is dependent on everything.
It is easy to do non-naive Bayesian learning by specifying a more complicated dependence structure.
See this tutorial http://izbicki.me/blog/markov-networks-monoids-and-futurama for more details on how.

> type NB = Bayes TH_income NBDist
> 
> type NBDist = (Multivariate Person 
>               '[ MultiCategorical '[String]
>                , Independent Categorical (Replicate 8 String)
>                , Independent Normal (Replicate 6 Double)
>                ] Double)

Our main function just loads the csv file and calls the group cross-validation function.
We have to print the mean and variance to force the result because of Haskell's laziness.

> main = do
>    [file,foldstr] <- getArgs
>    putStrLn $ "HLearn cross-validation of"++file++"with "++foldstr++" folds"
>    let numfolds = read foldstr :: Int
>    putStr "loading file... "
>    Right rawdata <- fmap (fmap V.toList . decode True) $ BS.readFile file
>         :: IO (Either String [Person])
>    putStrLn "done."
>    --let model = train rawdata :: NB
>    --let res= crossValidate_group (folds numfolds rawdata) (errorRate :: LossFunction NB)
>    let res= evalRandIO $ crossValidate_group (kfold numfolds) errorRate rawdata (undefined::NB)
>    putStrLn $ "mean = "++show (mean res)
>    putStrLn $ "variance = "++show (variance res)

Awesome!
