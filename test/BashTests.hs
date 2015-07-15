{-
 - This file is a hack so that we can run bash script tests using cabal.
 - See:  http://stackoverflow.com/questions/31213883/how-to-use-cabal-with-bash-tests
 -
 - Note that running `cabal test` will also run `cabal install`.
 -
 -}

import Control.Monad
import System.Exit
import System.Process

runBashTest :: String -> IO ()
runBashTest cmd = do
    putStr $ cmd ++ "..."
    ExitSuccess <- system cmd
    return ()

main = do
    system "cabal install"
    sequence_
        [ runBashTest $ "./test/allknn-verify/runtest.sh "
            ++ " " ++ dataset
            ++ " " ++ treetype
            ++ " " ++ foldtype
            ++ " " ++ rotate

        | dataset <-
            [ "./test/allknn-verify/dataset-10000x2.csv"
--             , "./test/allknn-verify/dataset-10000x20.csv"
--             , "./test/allknn-verify/mnist-10000.csv"
            ]
        , treetype <-
            [ "--treetype=simplified"
            , "--treetype=ancestor"
            ]
        , foldtype <-
            [ "--fold=foldsort"
            , "--fold=fold"
            ]
        , rotate <-
            [ "--rotate=norotate"
            , "--rotate=variance"
            ]
        ]
