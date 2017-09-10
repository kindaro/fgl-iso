module Data.Graph.Inductive.Isomorphism where

import Data.Graph.Inductive

iso :: DynGraph gr => gr a b -> gr a b -> Bool
iso x y
    | isEmpty x = isEmpty y
    | order x /= order y = False
    | size x /= size y = False
    | otherwise = iso' x y
    where

    -- | Deal with non-trivial cases: non-empty graphs of equal size & order.
    iso' :: DynGraph gr => gr a b -> gr a b -> Bool
    iso' x y = let n = node' . fst $ matchAny x in or $ do
        m <- nodes $ nfilter (\m -> match n x `similar` match m y) y
        return $ iso (delNode n x) (delNode m y)

        where
        similar :: DynGraph gr => Decomp gr a b -> Decomp gr a b -> Bool
        similar (Just n', x') (Just m', y') = indeg x n == indeg y m && outdeg x n == outdeg y m
            where
            x = n' & x'
            y = m' & y'
            n = node' n'
            m = node' m'

-- isol -- isomorphic up to label

-- isos -- generate all isomorphies (how would that look? as Sn?)
