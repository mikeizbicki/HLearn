module HLearn.Data.Graph
    where


import SubHask
import SubHask.Compatibility.HMatrix
import SubHask.TemplateHaskell.Deriving

import qualified Data.Vector.Generic as VG

import Data.List (reverse,take,permutations)
import Control.DeepSeq
import Data.List (lines,filter,head,words,sort,replicate,take,nubBy,zip,zip3)
import System.IO
import System.Directory

--------------------------------------------------------------------------------

class
    ( Metric v
    , Ord (Scalar v)
    , HasScalar v
    , ExpField (Scalar v)
    ) => KernelSpace v
        where
    kernel :: v -> v -> Scalar v

    selfKernel :: v -> Scalar v
    selfKernel v = kernel v v

kernelNorm :: KernelSpace v => v -> Scalar v
kernelNorm = sqrt . selfKernel

kernelDistance :: KernelSpace v => v -> v -> Scalar v
kernelDistance v1 v2 = sqrt $ selfKernel v1 + selfKernel v2 - 2* kernel v1 v2

---------------------------------------

-- | A "SelfKernel" precomputes the kernel applied to itself twice.
-- This is a common calculation in many kernelized algorithms, so this can greatly speed up computation.
-- newtype SelfKernel v = SelfKernel (Scalar v, v)
-- deriveHierarchy ''SelfKernel [''Ord,''Boolean]
-- deriveTypefamilies [''Scalar] ''SelfKernel
-- deriveSingleInstance ''SelfKernel ''Eq
-- deriveSingleInstance ''SelfKernel ''POrd
-- deriveSingleInstance ''SelfKernel ''Ord
-- deriveSingleInstance ''SelfKernel ''SupSemilattice
-- deriveSingleInstance ''SelfKernel ''InfSemilattice
-- deriveSingleInstance ''SelfKernel ''Lattic

data SelfKernel v = SelfKernel !(Scalar v) !v

type instance Logic (SelfKernel v) = Logic v

instance (NFData v, NFData (Scalar v)) => NFData (SelfKernel v) where
    rnf (SelfKernel k v) = deepseq k $ rnf v

instance (Show (Scalar v), Show v) => Show (SelfKernel v) where
    show (SelfKernel k v) = "SelfKernel "++show k++" "++show v

mkSelfKernel :: KernelSpace v => v -> SelfKernel v
mkSelfKernel v = SelfKernel (selfKernel v) v

type instance Scalar (SelfKernel v) = Scalar v
type instance Elem (SelfKernel v) = Elem v

instance Eq v => Eq_ (SelfKernel v) where
    (SelfKernel _ v1)==(SelfKernel _ v2) = v1==v2

instance (KernelSpace v, POrd v) => POrd_ (SelfKernel v) where
    inf (SelfKernel _ v1) (SelfKernel _ v2) = mkSelfKernel $ inf v1 v2

instance (KernelSpace v, Lattice_ v) => Lattice_ (SelfKernel v) where
    sup (SelfKernel _ v1) (SelfKernel _ v2) = mkSelfKernel $ sup v1 v2

instance (KernelSpace v, Ord v) => Ord_ (SelfKernel v)

instance KernelSpace v => KernelSpace (SelfKernel v) where
    kernel (SelfKernel _ v1) (SelfKernel _ v2) = kernel v1 v2
    selfKernel (SelfKernel k _) = k

instance KernelSpace v => Normed (SelfKernel v) where
    size = kernelNorm

instance KernelSpace v => Metric (SelfKernel v) where
    distance = kernelDistance

--------------------------------------------------------------------------------
-- Graph

-- type Graph = SelfKernel Graph_

data Graph = Graph
    { graph :: Graph_
    , memo :: [Double]
    }
    deriving (Show)

type instance Logic Graph = Bool

instance Eq_ Graph where
    g1==g2 = graph g1==graph g2

-- instance POrd_ Graph
-- instance Lattice_ Graph
-- instance Ord_ Graph

instance NFData Graph where
    rnf (Graph g m) = deepseq g $ rnf m

mkGraph :: Graph_ -> Graph
mkGraph g_ = Graph
    { graph = g_
    , memo = go lambdas one []
    }
    where

        go []     tm' ret = ret
--         go (0:xs) tm' ret = go xs tm'  $ 0:ret
--         go (x:xs) tm' ret = go xs tm'' $ ((startVec g_ <> (tm'' `mXv` stopVec g_))):ret
        go (0:xs) tm' ret = go xs tm'  $ ret+ [0]
        go (x:xs) tm' ret = go xs tm'' $ ret + [(startVec g_ <> (tm'' `mXv` stopVec g_))]
            where
                tm'' = tm' * transitionMatrix g_

instance Metric Graph where
    distance (Graph _ xs) (Graph _ ys) = sqrt $ sum
        [ lambda * (x-y)**2
        | (lambda,x,y) <- zip3 lambdas xs ys
        ]

type instance Scalar Graph = Double

lambdas = [1,1/2,1/4,1/8,1/16]

distance' g1 g2 = distance (graph g1) (graph g2)

-----------------------------

data Graph_ = Graph_
    { transitionMatrix :: !(Matrix Double)
    , startVec :: !(DynVector 0 Double)
    , stopVec :: !(DynVector 0 Double)
    }
    deriving (Show)

type instance Logic Graph_ = Bool

instance Eq_ Graph_ where
    g1==g2 = transitionMatrix g1==transitionMatrix g2
           &&startVec g1==startVec g2
           &&stopVec g1 ==stopVec g2

type instance Scalar Graph_ = Double

instance NFData Graph_ where
--     rnf g = ()
    rnf g = deepseq (transitionMatrix g)
          $ deepseq (startVec g)
          $ rnf (stopVec g)

-- instance POrd_ Graph_
-- instance Lattice_ Graph_ where
--     pcompare g1 g2 = pcompare (toVector $ transitionMatrix g1) (toVector $ transitionMatrix g2)
--
-- instance Ord_ Graph_

productGraph_ :: Graph_ -> Graph_ -> Graph_
productGraph_ g1 g2 = Graph_
    { transitionMatrix = transitionMatrix g1 >< transitionMatrix g2
    , startVec = toVector $ startVec g1 >< startVec g2
    , stopVec = toVector $ stopVec g1 >< stopVec g2
    }

instance KernelSpace Graph_ where
    kernel = mkKernelGraph_ lambdas

-- mkKernelGraph :: [Double] -> Graph -> Graph -> Double
-- mkKernelGraph xs (SelfKernel _ g1) (SelfKernel _ g2) = mkKernelGraph_ xs g1 g2

mkKernelGraph_ :: [Double] -> Graph_ -> Graph_ -> Double
mkKernelGraph_ xs g1 g2 = go xs one 0
    where
        gprod = productGraph_ g1 g2

        go []     tm' ret = ret
        go (0:xs) tm' ret = go xs tm'  $ ret
        go (x:xs) tm' ret = go xs tm'' $ ret + (x *. (startVec gprod <> (tm'' `mXv` stopVec gprod)))
            where
                tm'' = tm' * transitionMatrix gprod

-- mkDistanceGraph :: [Double] -> Graph -> Graph -> Double
-- mkDistanceGraph lambdas (SelfKernel _ g1) (SelfKernel _ g2)
--     = sqrt $ sum
-- --         [ mkKernelGraph_ lambdas g1 g1
-- --         + mkKernelGraph_ lambdas g2 g2
-- --         - mkKernelGraph_ lambdas g1 g2
-- --         - mkKernelGraph_ lambdas g1 g2
-- --
-- --         ]
--
-- --         [ (lambdas!(i-1))*
-- --             ( startVec g1' <> ((transitionMatrix g1'^^^i) `mXv` stopVec g1')
-- --             + startVec g2' <> ((transitionMatrix g2'^^^i) `mXv` stopVec g2')
-- --             - startVec gp <> ((transitionMatrix gp^^^i) `mXv` stopVec gp)
-- --             - startVec gp <> ((transitionMatrix gp^^^i) `mXv` stopVec gp)
-- --             )
-- --
-- --         | i <- [1..length lambdas::Int]
-- --         ]
-- --         where
-- --             g1'=productGraph_ g1 g1
-- --             g2'=productGraph_ g2 g2
-- --             gp=productGraph_ g1 g2
--
--         [ (lambdas!i)*
--             ( startVec g1 <> ((transitionMatrix g1^^^(i+1)) `mXv` stopVec g1)
--             - startVec g2 <> ((transitionMatrix g2^^^(i+1)) `mXv` stopVec g2)
--             ) **2
--         | i <- [0..length lambdas-1::Int]
--         ]

(^^^) :: Ring r => r -> Int -> r
(^^^) r 0 = one
(^^^) r 1 = r
(^^^) r i = r*(r^^^(i-1))

mag :: Graph_ -> Double
mag g = startVec g <> (transitionMatrix g `mXv` stopVec g)

instance Metric Graph_ where
    distance = kernelDistance

edgeList2UndirectedGraph :: Int -> [(Int,Int)] -> Graph
edgeList2UndirectedGraph numVertices edgeList = edgeList2Graph numVertices $ symmetrize edgeList

edgeList2Graph :: Int -> [(Int,Int)] -> Graph
-- edgeList2Graph numVertices edgeList = mkSelfKernel $ Graph_
edgeList2Graph numVertices edgeList = mkGraph $ Graph_
    { transitionMatrix = mat -- +one
    , startVec=VG.replicate numVertices $ 1/fromIntegral numVertices
    , stopVec=VG.replicate numVertices $ 1/fromIntegral numVertices
    }
    where
        mat = mkMatrix numVertices numVertices
            $ map (fromIntegral :: Int -> Double)
            $ edgeList2AdjList numVertices numVertices edgeList

edgeList2AdjList :: Int -> Int -> [(Int,Int)] -> [Int]
edgeList2AdjList w h xs = go 0 0 (sort xs)
    where
        go r c []           = replicate (w-c+(h-r-1)*w) 0
        go r c ((xr,xc):xs) = if r==xr && c==xc
            then 1:go r' c' xs
            else 0:go r' c' ((xr,xc):xs)
            where
                c' = (c+1) `mod` w
                r' = if c+1 ==c' then r else r+1

--         go i j []           ret = (reverse ret) ++ replicate (w-i+(h-j-1)*w) 0
--         go i j ((xi,xj):xs) ret = if i==xi && j==xj
--             then go i' j' xs            (1:ret)
--             else go i' j' ((xi,xj):xs)  (0:ret)
--             where
--                 j'=(j+1) `mod` w
--                 i'=if (j+1)==j' then i else i+1

symmetrize :: [(Int,Int)] -> [(Int,Int)]
symmetrize xs = sort $ go xs []
    where
        go []         ret = ret
        go ((i,j):xs) ret = if i==j
            then go xs $ (i,j):ret
            else go xs $ (i,j):(j,i):ret

--------------------------------------------------------------------------------
-- file IO

-- | helper for "loadDirectory"
isFileTypePLG :: FilePath -> Bool
isFileTypePLG filepath = take 4 (reverse filepath) == "glp."

-- | helper for "loadDirectory"
isNonemptyGraph :: Graph -> Bool
isNonemptyGraph (Graph v _) = VG.length (startVec v) > 0

-- | loads a file in the PLG data format into a graph
--
-- See: www.bioinformatik.uni-frankfurt.de/tools/vplg/ for a description of the file format
loadPLG
    :: Bool     -- ^ print debug info?
    -> FilePath -- ^ path of SUBDUE graph file
    -> IO Graph
loadPLG debug filepath = {-# SCC loadPLG #-} do
    filedata <- liftM lines $ readFile filepath

    let edgeList    = mkEdgeList filedata
        numEdges    = length edgeList
        numVertices = length $ mkVertexList filedata

    when debug $ do
        putStrLn $ filepath++"; num vertices = "++show numVertices++"; num edges = "++show numEdges

    let ret = edgeList2UndirectedGraph numVertices edgeList
    deepseq ret $ return ret

    where
        mkVertexList xs = filter (\x -> head x == "|") $ map words xs
        mkEdgeList xs = map (\["=",v1,"=",t,"=",v2] -> (read v1-1::Int,{-t,-}read v2-1::Int))
                      $ filter (\x -> head x == "=")
                      $ map words xs


-- | loads a file in the SUBDUE data format into a graph
--
-- FIXME: not implemented
loadSubdue
    :: Bool     -- ^ print debug info?
    -> FilePath -- ^ path of SUBDUE graph file
    -> IO Graph_
loadSubdue debug filepath = do
    file <- liftM lines $ readFile filepath
    let numVertices = getNumVertices file
        numEdges = getNumEdges file
    putStrLn $ "num vertices = " ++ show numVertices
    putStrLn $ "num edges = " ++ show numEdges
    undefined
    where
        getNumVertices xs = length $ filter (\x -> head x == "v") $ map words xs
        getNumEdges xs = length $ filter (\x -> head x == "u") $ map words xs
--     hin <- openFile filepath ReadMode
--     hClose hin
--     undefined


--------------------------------------------------------------------------------
-- tests

g1 = edgeList2UndirectedGraph 2 $ [(0,0)]
g2 = edgeList2UndirectedGraph 2 $ [(0,0),(0,1)]
g3 = edgeList2UndirectedGraph 2 $ [(0,0),(1,1)]
g4 = edgeList2UndirectedGraph 2 $ [(1,0)]
h1 = edgeList2UndirectedGraph 3 $ [(0,0),(1,1),(2,2)]
h2 = edgeList2UndirectedGraph 3 $ [(0,0),(0,1),(0,2)]
h3 = edgeList2UndirectedGraph 3 $ [(0,0),(0,1),(0,2),(2,2)]
h4 = edgeList2UndirectedGraph 3 $ [(1,1),(0,1),(0,2),(2,2)]

a = edgeList2UndirectedGraph 3 $ [(0,0),(0,1),(0,2)]
b = edgeList2UndirectedGraph 3 $ [(0,0),(0,2)]
c = edgeList2UndirectedGraph 3 $ [(0,0),(0,2),(1,1),(1,2)]
d = edgeList2UndirectedGraph 3 $ [(0,0),(0,2),(1,1)]
e = edgeList2UndirectedGraph 3 $ [(2,2),(1,1)]
f = edgeList2UndirectedGraph 3 $ [(0,0),(0,1),(0,2),(1,1),(1,2)]
g = edgeList2UndirectedGraph 3 $ [(0,0),(0,1),(0,2),(1,1),(1,2),(2,2)]
h = edgeList2UndirectedGraph 3 $ [(1,1)]
i = edgeList2UndirectedGraph 3 $ []

