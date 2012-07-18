module Base
    where
          
import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.Number.LogFloat

-------------------------------------------------------------------------------
-- data types

data DataItem = Discrete String
              | Continuous Double
              | Missing
    deriving (Read,Show,Eq)

type Probability = LogFloat
-- type Probability = Double

type TrainingDataSparse = [(Int,DataPointSparse)]
type DataPointSparse = [(Int,DataItem)]

data DataDesc = DataDesc
    { numLabels :: Int
    , numAttr :: Int
    }
    deriving (Eq,Read,Show)

instance Binary DataDesc where
    put desc = do
        put $ numLabels desc
        put $ numAttr desc
    get = liftM2 DataDesc get get

instance NFData DataDesc where
    rnf desc = seq (rnf $ numLabels desc) (rnf $ numAttr desc)
