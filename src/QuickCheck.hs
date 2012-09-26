{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Data.Number.LogFloat
import Test.QuickCheck hiding (sample,classify)
-- import Test.ClassLaws

import HMine.Base
import HMine.DataContainers
import HMine.DataContainers.DS_List
import HMine.Distribution
import HMine.Math.Algebra
import HMine.Math.Functors
import HMine.Math.TypeClasses
import HMine.Models.Distributions
import HMine.Models.DTree
import HMine.Models.NBayes
import HMine.Models.Ensemble
import HMine.Models.Ensemble.AdaBoost
import HMine.Models.Ensemble.Bagging

-------------------------------------------------------------------------------
-- Create arbitrary datapoints

arbitraryContinuous = liftM Continuous $ choose (-10000,10000)

arbitraryLDPS :: DataDesc Int -> Gen (LDPS Int)
arbitraryLDPS desc = do
    label <- choose (0,numLabels desc-1)
    dpL <- replicateM (numAttr desc) arbitraryContinuous
    return (label,zip [0..numAttr desc-1] dpL)

instance Arbitrary (DS_List Int (LDPS Int)) where
    arbitrary = do
        let numLabels = 2
        let numAttr = 3
        let dsDesc = DataDesc numLabels [0..numLabels-1] numAttr
        dsLen <- choose (5,10)
        let dsLabelL = [0..numLabels-1]
        
        dsL <- replicateM dsLen $ arbitraryLDPS dsDesc
        
        return $ DS_List
            { dsDesc = dsDesc
            , dsL = dsL
            , dsLen = dsLen
            }
        
-- arbitraryDS_List desc = do
-- --     numLabels <- choose (2,10)
-- --     numAttr <- choose (1,10)
-- --     let desc = DataDesc numLabels numAttr
--     csv <- forM [1..5] (\x -> csvLine desc)
--     return $ ds2intds $ csv2data csv
-- 
-- csvLine desc = do
--     label <- choose (0,(numLabels desc)-1)
--     attrL <- vector (numAttr desc) :: Gen [Double]
--     return $ (map show attrL)++[show label]

-------------------------------------------------------------------------------
-- properties

-- prop_dps dp = dp == (dpf2dps $ dps2dpf 10 dp)
-- prop_dpf dp = dp == (dps2dpf (length dp) $ dpf2dps dp)

---------------------------------------

prop_SemigroupAssociative (a,b,c) = (a<>b)<>c == a<>(b<>c)

---------------------------------------

-- prop_InverseSemigroup :: (Equal isg,InverseSemigroup isg) => (isg,isg) -> Bool
prop_InverseSemigroup (isg1,isg2) = isg1<>isg2<>(inverse isg2) == isg1

---------------------------------------

prop_TrainerSemigroup params (dps1,dps2) = runHMine 1 $ do
    model1  <- trainBatch params dps1
    model2  <- trainBatch params dps2
    model12 <- trainBatch params (dps1<>dps2)
    return $ model1<>model2==model12

prop_OnlineTrainer params (dps) = modelOffline==modelOnline
    where
        modelOnline  = runHMine 1 $ trainOnline params dps
        modelOffline = runHMine 1 $ trainBatch params dps

{-prop_MutableTrainer ::
    ( Eq model
    , DataSparse label ds (LDPS label)
    , MutableTrainer modelparams model modelST label
    , OnlineTrainer modelparams model label
    ) =>
    modelparams -> ds (LDPS label) -> Bool-}
prop_MutableTrainer params (dps) = modelPure==modelST
    where
        modelPure = runHMine 1 $ trainBatch params dps
        modelST   = {-runHMine 1 $-} trainST params dps
        
-- prop_ContinuousDistribution :: (Distribution dist Double,
--                               ContinuousDistribution dist Double) =>
--                              dist -> dist -> Bool
prop_ContinuousDistribution (dist1,dist2) = and $ map prop_intersectionpoint xs
       
    where
        epsilon = 0.01 :: Double
        xs = intersectionScaled (w1,dist1) (w2,dist2) :: [Double]
        
        w1=0.2
        w2=0.1
        
        prop_intersectionpoint x = 
            (abs $ w1*(fromLogFloat $ pdf dist1 x) - w2*(fromLogFloat $ pdf dist2 x)) <= epsilon
                

-------------------------------------------------------------------------------
-- tests

test_NBayes = do
    quickCheck (prop_TrainerSemigroup defNBayesParams :: (DS_List Int (LDPS Int), DS_List Int (LDPS Int)) -> Bool)
    quickCheck (prop_OnlineTrainer defNBayesParams :: (DS_List Int (LDPS Int)) -> Bool)
--     quickCheck (prop_MutableTrainer defNBayesParams :: (DS_List Int (LDPS Int)) -> Bool)

test_Boosting = do
    quickCheck (prop_TrainerSemigroup (defBaggingParams 10 10 defNBayesParams) :: (DS_List Int (LDPS Int), DS_List Int (LDPS Int)) -> Bool)
--     quickCheck (prop_OnlineTrainer (AdaBoostParams 10 defNBayesParams) :: (DS_List Int (LDPS Int)) -> Bool)

test_Bagging = do
    quickCheck (prop_TrainerSemigroup (defBaggingParams 10 10 defNBayesParams) :: (DS_List Int (LDPS Int), DS_List Int (LDPS Int)) -> Bool)
    quickCheck (prop_OnlineTrainer (defBaggingParams 10 10 defNBayesParams) :: (DS_List Int (LDPS Int)) -> Bool)
    
test_Gaussian = do
    quickCheck (prop_SemigroupAssociative :: (Gaussian Double,Gaussian Double,Gaussian Double) -> Bool)
    quickCheck (prop_InverseSemigroup :: (Gaussian Double,Gaussian Double) -> Bool)
--     quickCheck (prop_ContinuousDistribution :: (Gaussian Double,Gaussian Double) -> Bool)

---------------------------------------

-- test_DP = do
--     quickCheck prop_dps
--     quickCheck prop_dpf

-------------------------------------------------------------------------------
-- fixed stuff

dpdesc = DataDesc 2 [0,1] 3

-- dp 0 = (1::Int,[(0::Int,Continuous (-134.77947687658866)),(1::Int,Continuous 334.64191286414643),(2::Int,Continuous 762.7381029569324)])
dp 0 = (0::Int,[(0::Int,Continuous 6)   ,(1::Int,Continuous 180),(2::Int,Continuous 12),(3,Discrete "m")])
dp 1 = (0::Int,[(0::Int,Continuous 5.92),(1::Int,Continuous 190),(2::Int,Continuous 11),(3,Discrete "m")])
dp 2 = (0::Int,[(0::Int,Continuous 5.58),(1::Int,Continuous 170),(2::Int,Continuous 12),(3,Discrete "m")])
dp 3 = (0::Int,[(0::Int,Continuous 5.92),(1::Int,Continuous 165),(2::Int,Continuous 10),(3,Discrete "f")])
dp 4 = (1::Int,[(0::Int,Continuous 5)   ,(1::Int,Continuous 100),(2::Int,Continuous 6 ),(3,Discrete "m")])
dp 5 = (1::Int,[(0::Int,Continuous 5.5) ,(1::Int,Continuous 150),(2::Int,Continuous 8 ),(3,Discrete "m")])
dp 6 = (1::Int,[(0::Int,Continuous 5.42),(1::Int,Continuous 130),(2::Int,Continuous 7 ),(3,Discrete "f")])
dp 7 = (1::Int,[(0::Int,Continuous 5.75),(1::Int,Continuous 150),(2::Int,Continuous 9 ),(3,Discrete "f")])

dpsample = [(0::Int,Continuous 6),(1::Int,Continuous 130),(2::Int,Continuous 8){-,(3::Int,Continuous 22)-}]

td :: DS_List Int (LDPS Int)
td = DS_List
    { dsDesc = DataDesc 2 [0,1] 4
    , dsL = map dp [0..7]
    , dsLen = 8
    }

tdw :: DS_List Int (WLDPS Int)
tdw = DS_List
    { dsDesc = DataDesc 2 [0,1] 3
    , dsL = map (\x -> (dp x,1/8)) [0..7]
    , dsLen = 8
    }

tdempty :: DS_List Int (LDPS Int)
tdempty = DS_List
    { dsDesc = DataDesc 2 [0,1] 4
    , dsL = [(i,[])| i<-[0..7]]
    , dsLen = 8
    }

td2 :: DS_List Int (LDPS Int)
td2 = DS_List
    { dsDesc = DataDesc {numLabels = 3, labelL=[0,1,2], numAttr = 5}
    , dsL = [(1,[(0,Continuous 795.4165925800171),(1,Continuous 743.74693684771),(2,Continuous 438.5306366903055),(3,Continuous 511.01889752959414),(4,Continuous (-648.3542558864287))]),(0,[(0,Continuous (-235.05576388871498)),(1,Continuous 316.6606728425379),(2,Continuous (-154.35545015179537)),(3,Continuous (-847.1656966435592)),(4,Continuous 34.02047657205958)]),(0,[(0,Continuous (-673.0468167154877)),(1,Continuous (-262.7838516192967)),(2,Continuous 487.45947883791246),(3,Continuous (-164.87393384891732)),(4,Continuous 924.3608527822748)]),(2,[(0,Continuous 744.8068996105603),(1,Continuous 379.03060053327226),(2,Continuous (-830.2487583674458)),(3,Continuous 725.466442361298),(4,Continuous 882.9985213135933)]),(0,[(0,Continuous (-624.1232197781255)),(1,Continuous (-952.1182453781107)),(2,Continuous 70.83618938402446),(3,Continuous 684.1041108926163),(4,Continuous 5.943909167829474)]),(0,[(0,Continuous 69.07857704832554),(1,Continuous (-51.19876011719521)),(2,Continuous 18.72928637408802),(3,Continuous (-192.86800842647574)),(4,Continuous 671.0169143834278)])]
    , dsLen = 6
    }

td3 :: DS_List Int (LDPS Int)
td3 = DS_List 
    { dsDesc = DataDesc 2 [0,1] 3
--     , dsL = [(1,[(0,Continuous (-134.77947687658866)),(1,Continuous 334.64191286414643),(2,Continuous 762.7381029569324)]),(0,[(0,Continuous 5.92),(1,Continuous 190.0),(2,Continuous 11.0)]),(0,[(0,Continuous 5.58),(1,Continuous 170.0),(2,Continuous 12.0)]),(0,[(0,Continuous 5.92),(1,Continuous 165.0),(2,Continuous 10.0)]),(1,[(0,Continuous 5.0),(1,Continuous 100.0),(2,Continuous 6.0)]),(1,[(0,Continuous 5.5),(1,Continuous 150.0),(2,Continuous 8.0)]),(1,[(0,Continuous 5.42),(1,Continuous 130.0),(2,Continuous 7.0)]),(1,[(0,Continuous 5.75),(1,Continuous 150.0),(2,Continuous 9.0)])]
    , dsL = [(0,[(0,Continuous (-134.77947687658866)),(1,Continuous 334.64191286414643),(2,Continuous 762.7381029569324)])
            ,(1,[(0,Continuous 5.58),(1,Continuous 170.0),(2,Continuous 12.0)])
--             ,(1,[(0,Continuous (-654.9835607043104)),(1,Continuous 640.7851694453179),(2,Continuous (-58.360622400780926))])
--             ,(0,[(0,Continuous (-338.0952557447454)),(1,Continuous (-166.78854703114348)),(2,Continuous (-59.588291470930926))])
--             ,(1,[(0,Continuous 830.1448938572362),(1,Continuous (-168.01716977557146)),(2,Continuous 748.5134243366872)])
--             ,(0,[(0,Continuous 828.9119795768777),(1,Continuous 626.6439373260582),(2,Continuous 404.9048661183897)])
--             ,(1,[(0,Continuous (-352.1592402475055)),(1,Continuous 293.697457822308),(2,Continuous (-251.25439846231234))])
--             ,(0,[(0,Continuous (-695.8502912925106)),(1,Continuous (-373.2898248006709)),(2,Continuous 590.3652433066186)])
--             ,(0,[(0,Continuous 637.0847016308724),(1,Continuous 481.93636500201455),(2,Continuous (-564.6389470021772))])
--             ,(1,[(0,Continuous (-510.468115395059)),(1,Continuous (-672.984855642971)),(2,Continuous 422.10222362418335)])
--             ,(1,[(0,Continuous 337.4673952422104),(1,Continuous 313.67334531954293),(2,Continuous (-124.36890588663312))])
            ]
    , dsLen = 2
    }