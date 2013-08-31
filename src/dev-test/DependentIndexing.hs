{-# LANGUAGE TemplateHaskell #-}

import Data.DependentIndexing

data Funky = Funky
    { a :: String
    , b :: Int
    , c :: (String,Double)
    }

data Same = Same
    { s1 :: String
    , s2 :: String
    }

-- makeIndexData ''Funky
-- makeDependentIndexData ''Funky
-- makeDependentIndexClass ''Funky
makeIndex ''Funky
makeIndex ''Same
