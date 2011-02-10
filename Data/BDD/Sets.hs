{-# LANGUAGE ScopedTypeVariables #-}
module Data.BDD.Sets
    (encodeSet,
     decodeSet,
     encodeSingleton
    ) where

import Data.BDD.Internals
import Data.Set as Set
import Data.Bits
import Data.Foldable

encodeSet :: (Bits a,Monad m) => Int -> Set a -> BDDM s Int m (Tree s Int)
encodeSet off set
  | Set.null set = false
  | otherwise = do
    trees <- mapM (encodeSingleton off) (Set.toList set)
    foldlM (#||) (head trees) (tail trees)

decodeSet :: (Bits a,Ord a) => Int -> Tree s Int -> Set a
decodeSet off tree = decodeSet' 0 tree 0 Set.empty
  where
    decodeSet' :: (Bits a,Ord a) => Int -> Tree s Int -> a -> Set a -> Set a
    decodeSet' pos (Leaf _ v) value cur = if v
                                                 then fillRemaining pos value cur
                                                 else cur
    decodeSet' pos node@(Node _ sym l r) value cur
      | sym - off < 0 = let s1 = decodeSet' pos l value cur
                            s2 = decodeSet' pos r value s1
                        in s2
      | sym - off > bitSize value = fillRemaining pos value cur
      | pos < sym - off = let s1 = decodeSet' (pos+1) node (setBit value pos) cur
                              s2 = decodeSet' (pos+1) node value s1
                          in s2
      | otherwise = let s1 = decodeSet' (pos+1) l (setBit value pos) cur
                        s2 = decodeSet' (pos+1) r value s1
                    in s2
    
    fillRemaining :: (Bits a,Ord a) => Int -> a -> Set a -> Set a
    fillRemaining pos value cur
      | pos == bitSize value = Set.insert value cur
      | otherwise = let s1 = fillRemaining (pos+1) (setBit value pos) cur
                        s2 = fillRemaining (pos+1) value cur
                    in s2

encodeSingleton :: (Bits a,Monad m) => Int -> a -> BDDM s Int m (Tree s Int)
encodeSingleton off v = encodeSingleton' 0
  where
    encodeSingleton' n = if n == bitSize v
                            then true
                            else (do
                                     f <- false
                                     res <- encodeSingleton' (n+1)
                                     if testBit v n
                                       then node (n+off) res f
                                       else node (n+off) f res)