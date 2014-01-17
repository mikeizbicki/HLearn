module HLearn.DataStructures.CoverTree.Extras
    where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import HLearn.Algebra
import HLearn.DataStructures.CoverTree

instance (VUM.Unbox (Ring dp), VUM.Unbox dp) => VUM.Unbox (CoverTree' base VU.Vector VU.Vector () dp)

data instance VU.Vector (CoverTree' base childContainer nodeVvec tag dp) = TreeVector
    { uvec         :: !(VU.Vector (dp,Int,Ring dp, Ring dp, Ring dp))
    , vec_children :: !(V.Vector (childContainer (CoverTree' base childContainer nodeVvec tag dp)))
    , vec_nodeV    :: !(V.Vector (nodeVvec dp))
    , vec_tag      :: !(V.Vector tag)
    }

instance 
    ( VUM.Unbox (Ring dp)
    , VUM.Unbox dp
--     ) => VG.Vector VU.Vector (CoverTree' base childContainer nodeVvec tag dp) 
    ) => VG.Vector VU.Vector (CoverTree' base VU.Vector VU.Vector () dp) 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze v = do
        uvec' <- VG.unsafeFreeze $ uvecM v
        vec_children' <- VG.unsafeFreeze $ vecM_children v
        vec_nodeV' <- VG.unsafeFreeze $ vecM_nodeV v
        vec_tag' <- VG.unsafeFreeze $ vecM_tag v
        return $ TreeVector uvec' vec_children' vec_nodeV' vec_tag'

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw v = do
        uvecM' <- VG.unsafeThaw $ uvec v
        vecM_children' <- VG.unsafeThaw $ vec_children v
        vecM_nodeV' <- VG.unsafeThaw $ vec_nodeV v
        vecM_tag' <- VG.unsafeThaw $ vec_tag v
        return $ TreeMVector uvecM' vecM_children' vecM_nodeV' vecM_tag'
    
    {-# INLINE basicLength #-}
    basicLength v = VG.basicLength (uvec v)

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i len v = TreeVector
        { uvec = VG.unsafeSlice i len $ uvec v
        , vec_children = VG.unsafeSlice i len $ vec_children v
        , vec_nodeV = VG.unsafeSlice i len $ vec_nodeV v
        , vec_tag = VG.unsafeSlice i len $ vec_tag v
        }

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM v i = do
        (nodedp',level',weight',numdp',maxDescendentDistance') <- VG.basicUnsafeIndexM (uvec v) i
        children' <- VG.unsafeIndexM (vec_children v) i
        nodeV' <- VG.unsafeIndexM (vec_nodeV v) i
        tag' <- VG.unsafeIndexM (vec_tag v) i
        return $ Node nodedp' level' weight' numdp' maxDescendentDistance' children' nodeV' tag'

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy mv v = do
        VG.basicUnsafeCopy (uvecM mv) (uvec v)
        VG.basicUnsafeCopy (vecM_children mv) (vec_children v)
        VG.basicUnsafeCopy (vecM_nodeV mv) (vec_nodeV v)
        VG.basicUnsafeCopy (vecM_tag mv) (vec_tag v)
        
    {-# INLINE elemseq #-}
    elemseq v = seq

data instance VUM.MVector s (CoverTree' base childContainer nodeVvec tag dp) = TreeMVector
    { uvecM         :: !(VUM.MVector s (dp,Int,Ring dp, Ring dp, Ring dp))
    , vecM_children :: !(VM.MVector s (childContainer (CoverTree' base childContainer nodeVvec tag dp)))
    , vecM_nodeV    :: !(VM.MVector s (nodeVvec dp))
    , vecM_tag      :: !(VM.MVector s tag)
    }

instance
    ( VUM.Unbox (Ring dp)
    , VUM.Unbox dp
    ) => VGM.MVector VUM.MVector (CoverTree' base VU.Vector VU.Vector () dp) 
        where
    {-# INLINE basicLength #-} 
    basicLength v = VGM.basicLength (uvecM v)

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i len v = TreeMVector
        { uvecM = VGM.unsafeSlice i len $ uvecM v
        , vecM_children = VGM.unsafeSlice i len $ vecM_children v
        , vecM_nodeV = VGM.unsafeSlice i len $ vecM_nodeV v
        , vecM_tag = VGM.unsafeSlice i len $ vecM_tag v
        }

    {-# INLINE basicOverlaps #-}
    basicOverlaps v1 v2 
        =  VGM.basicOverlaps (uvecM v1) (uvecM v2)
        || VGM.basicOverlaps (vecM_children v1) (vecM_children v2)
        || VGM.basicOverlaps (vecM_nodeV v1) (vecM_nodeV v2)
        || VGM.basicOverlaps (vecM_tag v1) (vecM_tag v2)
    
    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew i = do
        uvecM' <- VGM.unsafeNew i
        vecM_children' <- VGM.unsafeNew i
        vecM_nodeV' <- VGM.unsafeNew i
        vecM_tag' <- VGM.unsafeNew i
        return $ TreeMVector uvecM' vecM_children' vecM_nodeV' vecM_tag'

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead v i = do
        (nodedp',level',weight',numdp',maxDescendentDistance') <- VGM.basicUnsafeRead (uvecM v) i
        children' <- VGM.unsafeRead (vecM_children v) i
        nodeV' <- VGM.unsafeRead (vecM_nodeV v) i
        tag' <- VGM.unsafeRead (vecM_tag v) i
        return $ Node nodedp' level' weight' numdp' maxDescendentDistance' children' nodeV' tag'

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite v i ct = do
        VGM.unsafeWrite (uvecM v) i (nodedp ct, level ct, weight ct, numdp ct, maxDescendentDistance ct)
        VGM.unsafeWrite (vecM_children v) i (children ct)
        VGM.unsafeWrite (vecM_nodeV v) i (nodeV ct)
        VGM.unsafeWrite (vecM_tag v) i (tag ct)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy v1 v2 = do
        VGM.basicUnsafeCopy (uvecM v1) (uvecM v2)
        VGM.basicUnsafeCopy (vecM_children v1) (vecM_children v2)
        VGM.basicUnsafeCopy (vecM_nodeV v1) (vecM_nodeV v2)
        VGM.basicUnsafeCopy (vecM_tag v1) (vecM_tag v2)

    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove v1 v2 = do
        VGM.basicUnsafeMove (uvecM v1) (uvecM v2)
        VGM.basicUnsafeMove (vecM_children v1) (vecM_children v2)
        VGM.basicUnsafeMove (vecM_nodeV v1) (vecM_nodeV v2)
        VGM.basicUnsafeMove (vecM_tag v1) (vecM_tag v2)

    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow v i = do
        uvecM' <- VGM.basicUnsafeGrow (uvecM v) i
        vecM_children' <- VGM.basicUnsafeGrow (vecM_children v) i
        vecM_nodeV' <- VGM.basicUnsafeGrow (vecM_nodeV v) i
        vecM_tag' <- VGM.basicUnsafeGrow (vecM_tag v) i
        return $ TreeMVector uvecM' vecM_children' vecM_nodeV' vecM_tag'

