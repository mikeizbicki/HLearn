{-# LANGUAGE TemplateHaskell #-}

module HLearn.Algebra.Types.HListTH
    where

import Language.Haskell.TH.Syntax

makeNat1Instance :: Int -> Q [Dec]
makeNat1Instance i = return [ TySynInstD (mkName "ToNat1") [ConT $ mkName $ show i] (ConT $ mkName "Zero") ] 

--             [ TySynInstD (mkName "IndexType"  ) [ConT datatype] (ConT $ mkName $ "I_"++nameBase datatype)
