module Data.BDD.Solutions
    (solutions)
    where

import Data.BDD.Internals
import Data.Set as Set
import Data.Map as Map

solutions :: (Ord a,Monad m) => Set a -> Tree s a -> BDDM s a m (Set (Map a Bool))
solutions vars tree = do
  res <- solutions' vars tree
  return $ Set.fromList $ concat $ 
         fmap (\sol -> let missing = Set.difference vars (Map.keysSet sol)
                      in if Set.null missing
                         then [sol]
                         else [ Map.union sol (Map.fromList p) | p <- sols (Set.toList missing) ]
              ) (Set.toList res)

sols []     = [[]]
sols (v:vs) = let rest = sols vs
                  s1 = fmap ((v,False):) rest
                  s2 = fmap ((v,True):) rest
              in s1++s2

solutions' :: (Ord a,Monad m) => Set a -> Tree s a -> BDDM s a m (Set (Map a Bool))
solutions' vars (Leaf _ False)   = return Set.empty
solutions' vars (Leaf _ True)    = return $ Set.singleton $ Map.empty
solutions' vars (Node _ sym l r)
    | Set.member sym vars = do
        ls <- solutions' vars l
        rs <- solutions' vars r
        return $ Set.union (Set.map (Map.insert sym True) ls)
                           (Set.map (Map.insert sym False) rs)
    | otherwise = do
        ls <- solutions' vars l
        rs <- solutions' vars r
        return $ Set.union ls rs
