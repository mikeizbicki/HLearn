module HLearn.Data.SpaceTree.Diagrams
    where

import qualified Prelude as P

import SubHask

import HLearn.Data.SpaceTree
import HLearn.Models.Distributions

import Diagrams.Prelude ()
import qualified Diagrams.Prelude as D
import Diagrams.Backend.SVG hiding (size)

--------------------------------------------------------------------------------

{-
-- drawCT ::
--     ( ValidCT exprat childC leafC dp
--     , VG.Vector childC (QDiagram SVG R2 Any)
--     , Integral (Scalar (leafC dp))
--     , Integral (Scalar (childC (CoverTree_ exprat childC leafC dp)))
--     ) => P.FilePath
--       -> CoverTree_ exprat childC leafC dp
--       -> IO ()
drawCT path ct = renderSVG path (Dims 500 300) (diagramCT_ 0 ct)


-- diagramCT node = diagramCT_ 0 node

-- type instance Scalar R2 = Double

-- diagramCT_ ::
--     ( ValidCT exprat childC leafC dp
--     ) => Int
--       -> CoverTree_ exprat childC leafC dp
--       -> Diagram a R2
diagramCT_ (depth::Int) tree
    = mkConnections $
        ( named (label++show depth) $ fontSize (Global 0.01) $
            (
                (text label D.<> strutY 0.5)
            === (text (show (sepdist tree)) D.<> strutY 0.5)
            -- === (text (show (maxDescendentDistance tree)) <> strutY 0.5)
            )
        D.<> circle 1 # fc nodecolor
        )
    === (pad 1.05 $ centerName (label++show (depth+1)) $
        VG.foldr (|||) mempty $ VG.map (diagramCT_ (depth+1)) $ children tree)

    where
        label = intShow $ nodedp tree
        nodecolor = if ctBetterMovableNodes tree==0 --nodeWeight tree > 0
            then red
            else lightblue

        mkConnections =
            D.connect (label++show depth) (label++show (depth+1))
            . apList (fmap
                (\key -> D.connect (label++show depth) (intShow key++show (depth+1)))
                (map nodedp $ toList $ children tree)
                )

centerName name = withName name $ \b a -> moveOriginTo (location b) a

apList :: [a -> a] -> a -> a
apList [] a = a
apList (x:xs) a = apList xs (x a)

intShow :: Show a => a -> String
intShow a = P.filter go $ show a
    where
        go x
            | x=='.' = True
            | x==',' = True
            | x=='-' = True
            | x=='1' = True
            | x=='2' = True
            | x=='3' = True
            | x=='4' = True
            | x=='5' = True
            | x=='6' = True
            | x=='7' = True
            | x=='8' = True
            | x=='9' = True
            | x=='0' = True
            | otherwise = False

-}
