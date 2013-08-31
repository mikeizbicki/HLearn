module HLearn.DataStructures.BinarySearchTree
    where
    
import Control.Applicative
import qualified Data.Foldable as F
import Debug.Trace
import GHC.TypeLits
import qualified Data.Vector as V

import qualified Control.ConstraintKinds as CK

import HLearn.Algebra
import HLearn.Models.Distributions

-------------------------------------------------------------------------------
-- data types

data BST a = Leaf | Node 
    { val :: a 
    , left :: BST a
    , right :: BST a
    }
    deriving (Read,Show,Eq,Ord)
    
insert :: (Ord a) => a -> BST a -> BST a
insert a Leaf = Node a Leaf Leaf
insert a bst 
    | a > val bst = bst { right = insert a $ right bst }
    | a < val bst = bst { left = insert a $ left bst }
    
delete :: (Ord a) => a -> BST a -> BST a
delete a Leaf = Leaf
delete a (Node v l r)
    | a < v = Node v (delete a l) r
    | a > v = Node v l (delete a r)
    | l==Leaf && r==Leaf = Leaf
    | l==Leaf = r
    | r==Leaf = l
    | otherwise = takeLeft
        where
            takeLeft = Node (biggest l) (deleteBiggest l) r

height :: BST a -> Int
height Leaf = 0
height (Node v l r) = 1 + max (height l) (height r)

balanceFactor :: BST a -> Int
balanceFactor Leaf = 0
balanceFactor (Node v l r) = height l - height r

biggest :: BST a -> a
biggest (Node v l Leaf) = v
biggest (Node v l r) = biggest r

smallest :: BST a -> a
smallest (Node v Leaf r) = v
smallest (Node v l r) = smallest l

deleteBiggest :: BST a -> BST a
deleteBiggest (Node v l Leaf) = l
deleteBiggest (Node v l r) = Node v l $ deleteBiggest r

deleteSmallest :: BST a -> BST a
deleteSmallest (Node v Leaf r) = r
deleteSmallest (Node v l r) = Node v (deleteSmallest l) r

-------------------------------------------------------------------------------
-- Algebra

-- instance Monoid (BST a) where
--     mempty = Node Leaf Leaf
--     mappend x y = 

---------------------------------------

instance F.Foldable BST where
    foldr f b Leaf = b
    foldr f b bst = F.foldr f (f (val bst) (F.foldr f b $ right bst)) $ left bst

-------------------------------------------------------------------------------
-- Training

