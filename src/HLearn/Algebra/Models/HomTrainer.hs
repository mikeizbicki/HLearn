-- | Every model in the HLearn library is an instance of the 'HomTrainer' type class.  This ensures that the batch trainer is a monoid homomorphism.  This is a restrictive condition that not all learning models satisfy; however, it is useful for two reasons.  First, this property lets us easily derive three important functions for machine learning algorithms: online trainers, parallel trainers, and fast cross-validation algorithms.  Second, many popular algorithms (or variants on them) satisfy the condition and are implemented in the library.
--
-- For a full theoretical description of the 'HomTrainer' class, see the paper: <INSERT HERE>
--
-- Unfortunately, the class hierarchy here is slightly more complicated.  In the paper, we assume that all parameters for a model can be included in the model's type.  Currently, however, this is not possible in Haskell, so every model must also have a data type that describes it's parameters.  This is the purpose of the 'ModelParams' class.  Most models have either no parameters, or reasonable defaults, and so their parameters are instances of the 'DefaultParams' class.

module HLearn.Algebra.Models.HomTrainer
    (     
    -- * HomTrainer
    HomTrainer (..)
    , WeightedHomTrainer (..)
    , NumDP(..)

    , Weighted

    -- * useful functions
    , sub1dp
    , subBatch
    , sub1dpW
    , subBatchW
    
    )
    where
          
import qualified Control.ConstraintKinds as CK
import Data.Foldable
          
import HLearn.Algebra.Functions
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules

-------------------------------------------------------------------------------
-- NumDP

-- | numdp returns the number of data points that the model has been trained on
class NumDP model where
    numdp :: model -> Scalar model

-------------------------------------------------------------------------------
-- HomTrainer

-- | A minimal complete definition of the class is the singleton trainer 'train1dp\''
class 
    ( Monoid model
    ) => HomTrainer model 
        where
    
    type Datapoint model

    -- | The singleton trainer
    {-# INLINE train1dp #-}
    train1dp :: Datapoint model -> model
    train1dp = unbatch train
    
    -- | The batch trainer
    {-# INLINE train #-}
    train :: (Foldable container) => container (Datapoint model) -> model
    train = batch train1dp

    -- | The online trainer
    {-# INLINE add1dp #-}
    add1dp :: model -> Datapoint model -> model
    add1dp model = online train1dp model
    
    -- | The batch online trainer; will be more efficient than simply calling 'add1dp' for each element being added
    {-# INLINE addBatch #-}
    addBatch :: (Foldable container) => model -> container (Datapoint model) -> model
    addBatch model = online train model

    -- | CK methods take advantage of the ContraintKinds extension to allow containers that require constraints.  In particular, they allow the use of Unboxed Vectors, which can improve performance.
    {-# INLINE trainCK #-}
    trainCK ::     
        ( CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container (Datapoint model)
        ) => container (Datapoint model) -> model
    trainCK = batchCK train1dp

    {-# INLINE addBatchCK #-}
    addBatchCK ::
        ( CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container (Datapoint model)
        ) =>  model -> container (Datapoint model) -> model
    addBatchCK model = online trainCK model
    

-------------------------------------------------------------------------------
-- WeightedHomTrainer

type Weighted dp = (Scalar dp, dp)

class 
    ( HomTrainer model
    , Module model
    , Scalar (Datapoint model) ~ Scalar model
    ) => WeightedHomTrainer model where
        
    train1dpW :: Weighted (Datapoint model) -> model
    train1dpW (r,dp) = r .* train1dp dp
    
    trainW :: (Foldable container) => container (Weighted (Datapoint model)) -> model
    trainW = batch train1dpW

    add1dpW :: model -> Weighted (Datapoint model) -> model
    add1dpW = online $ unbatch $ offline addBatchW
    
    addBatchW :: (Foldable container) => model -> container (Weighted (Datapoint model)) -> model
    addBatchW = online trainW
    
instance (Module model, HomTrainer model, Scalar (Datapoint model) ~ Scalar model) => WeightedHomTrainer model
    
-------------------------------------------------------------------------------
-- helper functions

-- | subtracts a single data point from the model
sub1dp :: (Group model, HomTrainer model) => model -> Datapoint model -> model
sub1dp m dp = m <> inverse (train1dp dp)

-- | subtracts a multiple data point from the model
subBatch :: (Group model, HomTrainer model, Foldable container, Functor container) => 
    model -> container (Datapoint model) -> model
subBatch m dpL = m <> inverse (train dpL)

-- | subtracts a single weighted data point from the model
sub1dpW :: (Group model, WeightedHomTrainer model) => model -> Weighted (Datapoint model) -> model
sub1dpW m dp = m <> inverse (train1dpW dp)

-- | subtracts multiple weighted data points from the model
subBatchW :: (Group model, WeightedHomTrainer model, Foldable container, Functor container) => 
    model -> container (Weighted (Datapoint model)) -> model
subBatchW m dpL = m <> inverse (trainW dpL)
