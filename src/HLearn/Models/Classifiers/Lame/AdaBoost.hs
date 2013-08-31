module HLearn.Models.Classifiers.Lame.AdaBoost
    where

import HLearn.Algebra
import HLearn.Models.Classification
import HLearn.Models.Ensemble
import qualified Control.ConstraintKinds as CK

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-------------------------------------------------------------------------------
-- AdaBoost

data AdaBoostParams baseparams prob = AdaBoostParams 
    { baseparams :: baseparams
    }
    deriving (Read,Show,Eq,Ord)

-- instance Model (AdaBoostParams basemodelparams prob) (Ensemble (AdaBoostParams basemodelparams prob) model prob) where
instance (Eq baseparams) => Model (AdaBoostParams baseparams prob) (Ensemble (AdaBoostParams baseparams prob) model prob) where
    getparams (SGJust ens') = params ens'
    
-------------------------------------------------------------------------------
-- Training

instance 
    ( Floating prob
    , Ord label
    , LameTrainer baseparams V.Vector (prob,(label,attr)) basemodel
    , ProbabilityClassifier basemodel attr label prob
    , ProbabilityClassifier (Ensemble (AdaBoostParams baseparams prob) basemodel prob) attr label prob
    ) => LameTrainer 
        (AdaBoostParams baseparams prob) 
        V.Vector
        (label,attr) 
        (Ensemble (AdaBoostParams baseparams prob) basemodel prob) 
            where
    
    lame_train' params ds = go 0 _D0 _F0
        where
            m = V.length ds
            _y = V.map fst ds
            _x = V.map snd ds
            _D0 = V.replicate m (1/fromIntegral m) :: V.Vector prob
            _F0 = SGJust $ Ensemble' params mempty
            
            cost  x =  exp(-x)
            cost' x = -exp(-x)
            
            go :: Int -> (V.Vector prob) -> (Ensemble (AdaBoostParams baseparams prob) basemodel prob) -> (Ensemble (AdaBoostParams baseparams prob) basemodel prob) 
            go itr _D _F = 
                let f_t1 = lame_train' (baseparams params) (V.zip _D0 ds) :: basemodel
                    _y_t1 = V.map (classify f_t1) _x
                    err  = V.sum $ V.map (\(y0,y_t1) -> indicator $ y0==y_t1) $ V.zip _y _y_t1 
                    w_t1 = (1/2)*(log $ (1-err)/err) :: prob
                    _F_t1 = _F |> (w_t1,f_t1)
                    
                    _D_t1_raw = V.map (\(y,x) -> cost' $ bool2num $ y==classify _F_t1 x) $ V.zip _y _x
                    _D_t1_sum = V.sum _D_t1_raw
                    _D_t1     = V.map (\d -> d/_D_t1_sum) _D_t1_raw
                in if itr<20
                    then go (itr+1) _D_t1 _F_t1
                    else _F_t1
                
