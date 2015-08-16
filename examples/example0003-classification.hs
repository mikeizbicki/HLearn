{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

import SubHask
import SubHask.Algebra.Array
import SubHask.Algebra.Vector
import SubHask.Algebra.Container

import HLearn.Data.LoadData
import HLearn.Classifiers.Linear
import HLearn.History

import qualified Prelude as P
import System.IO

--------------------------------------------------------------------------------

main = do
    xs :: BArray (Labeled' (SVector "dyn" Double) (Lexical String))
       <- loadCSVLabeled' 0 "datasets/csv/uci/wine.csv"
       -- <- loadCSVLabeled' 8 "datasets/csv/uci/pima-indians-diabetes.csv"

    glm <- runHistory
        ( (displayFilter (maxReportLevel 2) dispIteration)
        + summaryTable
        )
        $ trainLogisticRegression 1e-3 xs

    putStrLn $ "loss_01       = "++show (validate loss_01       (toList xs) glm)
    putStrLn $ "loss_logistic = "++show (validate loss_logistic (toList xs) glm)
    putStrLn $ "loss_hinge    = "++show (validate loss_hinge    (toList xs) glm)

--     putStrLn ""
--     print $ show $ weights glm!Lexical "1"
--     print $ show $ weights glm!Lexical "2"
--     print $ show $ weights glm!Lexical "3"

    putStrLn "done."
