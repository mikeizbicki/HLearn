{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Main
    where

import HLearn.Data.SpaceTree.CoverTree

import SubHask
import SubHask.Algebra.Container
import SubHask.Algebra.Vector
import SubHask.TemplateHaskell.Test

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Runners.Console
import Test.Framework.Runners.Options

--------------------------------------------------------------------------------

main = defaultMainWithOpts
    [ testGroup "CoverTree_"
        [ $( mkSpecializedClassTests [t| UCoverTree (UVector "dyn" Float) |] [ ''Constructible ] )
        , $( mkSpecializedClassTests [t| BCoverTree (UVector "dyn" Float) |] [ ''Constructible ] )
        ]
    ]
    $ RunnerOptions
        { ropt_threads          = Nothing
        , ropt_test_options     = Nothing
        , ropt_test_patterns    = Nothing
        , ropt_xml_output       = Nothing
        , ropt_xml_nested       = Nothing
        , ropt_color_mode       = Just ColorAlways
        , ropt_hide_successes   = Just True
        , ropt_list_only        = Just True
        }
