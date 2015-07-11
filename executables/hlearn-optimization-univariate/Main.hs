{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

import SubHask
import SubHask.Algebra.Vector
import SubHask.Category.Trans.Derivative

import HLearn.History
import HLearn.Optimization.Univariate

import System.IO

--------------------------------------------------------------------------------

x0 :: OrdField a => a
x0 = (-5)

optmethod :: OrdField a => (a -> a) -> a
optmethod f = x1 $ evalHistory $ fminuncM_brent (maxIterations 20 || noProgress) x0 (return . f)

--------------------------------------------------------------------------------

main = do

    let f_slopes :: Float    = optmethod slopes
        d_slopes :: Double   = optmethod slopes
        r_slopes :: Rational = optmethod slopes

    putStrLn $ "f_slopes = " ++ show f_slopes
    putStrLn $ "d_slopes = " ++ show d_slopes
    putStrLn $ "r_slopes = " ++ show r_slopes

    let f_slopesWithDiscontinuity :: Float    = optmethod slopesWithDiscontinuity
        d_slopesWithDiscontinuity :: Double   = optmethod slopesWithDiscontinuity
        r_slopesWithDiscontinuity :: Rational = optmethod slopesWithDiscontinuity

    putStrLn $ "f_slopesWithDiscontinuity = " ++ show f_slopesWithDiscontinuity
    putStrLn $ "d_slopesWithDiscontinuity = " ++ show d_slopesWithDiscontinuity
    putStrLn $ "r_slopesWithDiscontinuity = " ++ show r_slopesWithDiscontinuity

    putStrLn "done."
