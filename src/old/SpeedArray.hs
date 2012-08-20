module Main
    where

import Control.DeepSeq
import Criterion.Main
import Data.Array
import Data.Vector

instance (NFData b) => NFData (Vector b) where
--     rnf x = rnf (bounds x, Data.Array.elems x)
--     rnf vec = Data.Vector.map rnf vec `seq` ()
    rnf vec = rnf $ toList vec

testArray2d :: (Int,Int) -> Array (Int,Int) Int
testArray2d (xdim,ydim) = array ((0,0),(xdim-1,ydim-1)) [((x,y),0) | x<-[0..xdim-1], y<-[0..ydim-1]]

createVector2d :: (Int,Int) -> Vector (Vector Int)
createVector2d (xdim,ydim) = fromList $ [fromList [0 | y<-[0..ydim-1]] | x<-[0..xdim-1]]

main = defaultMain 
    [ bgroup "createArray2d" 
        [ bench "(100,100)" $ nf testArray2d (100,100)
        , bench "(100,200)" $ nf testArray2d (100,200)
        , bench "(100,300)" $ nf testArray2d (100,300)
        , bench "(100,400)" $ nf testArray2d (100,400)
        , bench "(100,500)" $ nf testArray2d (100,500)
        ]
    , bgroup "createVector2d" 
        [ bench "(100,100)" $ nf createVector2d (100,100)
        , bench "(100,200)" $ nf createVector2d (100,200)
        , bench "(100,300)" $ nf createVector2d (100,300)
        , bench "(100,400)" $ nf createVector2d (100,400)
        , bench "(100,500)" $ nf createVector2d (100,500)
        ]
    ]