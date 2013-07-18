{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Data.Semigroup

class Model params model | params -> model, model -> params

data ContainerParams params = ContainerParams params

data Container' params model = Container'
    { baseparams :: params
    , basemodel  :: model
    }
    
instance (Semigroup model) => Semigroup (Container' params model) where
    c1 <> c2 = c1 { basemodel = basemodel c1 <> basemodel c2 }
    
-- type Container model = forall params . Container' params model

instance (Model params model) => Model (ContainerParams params) (Container' params model)

-- instance (Model params model) => Model (ContainerParams params) (Container model)

newtype Container model = Container (forall params . Model params model => Container' params model)
    deriving (Semigroup)