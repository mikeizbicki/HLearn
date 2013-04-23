{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Displays Multivariate dependencies

module HLearn.Models.Distributions.Visualization.Graphviz
    ( MultivariateLabels (..)
    , MarkovNetwork (..)
    )
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Multivariate.Interface
import HLearn.Models.Distributions.Multivariate.Internal.CatContainer
import HLearn.Models.Distributions.Multivariate.Internal.Container

import Data.GraphViz.Exception
import Data.GraphViz hiding (graphToDot)
import Data.GraphViz.Attributes.Complete{-( Attribute(RankDir, Splines, FontName)
                                        , RankDir(FromLeft), EdgeType(SplineEdges))-}
import Control.Arrow(second)
import GHC.TypeLits

-------------------------------------------------------------------------------
-- clases

class (Trainable datatype) => MultivariateLabels datatype where
    getLabels :: datatype -> [String]
                       
class (MultivariateLabels (Datapoint dist)) => MarkovNetwork dist where
    graphL :: dist -> [String] -> [(String,[String])]
    
    plotNetwork :: FilePath -> dist -> IO Bool
    plotNetwork file dist = graphToDotPng file $ graphL dist $ getLabels (undefined :: Datapoint dist)
    
-------------------------------------------------------------------------------
-- instances

instance 
    ( MultivariateLabels datapoint
    ) => MarkovNetwork (Multivariate datapoint '[] prob) 
        where
    graphL _ labels = []

instance 
    ( MultivariateLabels datapoint
    , MarkovNetwork (Multivariate datapoint xs prob)
    ) => MarkovNetwork (Multivariate datapoint ( ('[]) ': xs) prob) 
        where
    graphL _ labels = graphL (undefined :: Multivariate datapoint xs prob) labels

instance 
    ( MultivariateLabels datapoint
    , MarkovNetwork (Multivariate datapoint ( ys ': xs) prob)
    ) => MarkovNetwork (Multivariate datapoint ( (CatContainer label ': ys) ': xs) prob) 
        where
    graphL _ labels = (head labels, tail labels)
                    : (graphL (undefined :: Multivariate datapoint ( ys ': xs) prob) (tail labels))

instance 
    ( MultivariateLabels datapoint
    , MarkovNetwork (Multivariate datapoint (ys ': xs) prob) 
    ) => MarkovNetwork (Multivariate datapoint ( (Container dist label ': ys) ': xs) prob) 
        where
    graphL _ l = (head l,[]):(graphL (undefined::Multivariate datapoint (ys ': xs) prob) (tail l))

instance 
    ( MultivariateLabels datapoint
    , SingI (Length labelL)
    , MarkovNetwork (Multivariate datapoint ( ys ': xs) prob) 
    ) => MarkovNetwork (Multivariate datapoint ( (MultiContainer dist (labelL:: [*]) ': ys) ': xs) prob) 
        where
    graphL _ l = go (take n l) ++ (graphL (undefined ::  Multivariate datapoint ( ys ': xs ) prob) $ drop n l)
        where
            go [] = []
            go (x:xs) = (x,xs):(go xs)
              
            n = fromIntegral $ fromSing $ (sing :: Sing (Length labelL))

-------------------------------------------------------------------------------
-- Graphviz helpers

---------------------------------------
-- These functions are taken from the graphviz tutorial at:
-- http://ivanmiljenovic.wordpress.com/2011/10/16/graphviz-in-vacuum/

graphToDot :: (Ord a) => [(a, [a])] -> DotGraph a
graphToDot = graphToDotParams vacuumParams
 
graphToDotParams :: (Ord a, Ord cl) => GraphvizParams a () () cl l -> [(a, [a])] -> DotGraph a
graphToDotParams params nes = graphElemsToDot params ns es
  where
    ns = map (second $ const ()) nes
    es = concatMap mkEs nes
    mkEs (f,ts) = map (\t -> (f,t,())) ts
 
------------------------------------------------
 
vacuumParams :: GraphvizParams a () () () ()
vacuumParams = defaultParams { globalAttributes = gStyle }
 
gStyle :: [GlobalAttributes]
gStyle = [ GraphAttrs [RankDir FromLeft, {-Splines SplineEdges, -}FontName "courier", Layout Fdp]
         , NodeAttrs  [textLabel "\\N", shape PlainText, fontColor Blue, Shape Circle]
         , EdgeAttrs  [color Black, Dir NoDir]
         ]
         
graphToDotPng :: FilePath -> [(String,[String])] -> IO Bool
graphToDotPng fpre g = handle (\(e::GraphvizException) -> return False)
                       $ addExtension (runGraphviz (graphToDot g)) Png fpre >> return True
