{-# LANGUAGE TemplateHaskell #-}

import Data.DependentIndexing

data Funky = Funky
    { a :: String
    , b :: Int
    , c :: (String,Double)
    }

makeIndexData ''Funky
makeDependentIndexData ''Funky
makeDependentIndexClass ''Funky
