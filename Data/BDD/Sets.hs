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

encodeSet :: (Bits a,Ord a,Monad m) => Int -> Set a -> BDDM s Int m (Tree s Int)
encodeSet off (set::Set a) = encodeSet' (bitSize (undefined::a) - 1) set
  where
    --encodeSet' :: (Bits a,Ord a,Monad m) => Int -> Set a -> BDDM s Int m (Tree s Int)
    encodeSet' pos (elems::Set a)
      | Set.null elems = false
      | pos == -1 = true
      | otherwise = let (l_els,r_els) = Set.partition (\x -> testBit x pos) elems
                    in do
                      t1 <- encodeSet' (pos-1) l_els
                      t2 <- encodeSet' (pos-1) r_els
                      node ((bitSize (undefined::a))-1-pos+off) t1 t2

decodeSet :: (Bits a,Ord a) => Int -> Tree s Int -> Set a
decodeSet off tree = let p = 0
                     in decodeSet' ((bitSize p) - 1) tree p Set.empty
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
      | (bitSize value)-1-pos < sym - off = let s1 = decodeSet' (pos-1) node (setBit value pos) cur
                                                s2 = decodeSet' (pos-1) node value s1
                                            in s2
      | otherwise = let s1 = decodeSet' (pos-1) l (setBit value pos) cur
                        s2 = decodeSet' (pos-1) r value s1
                    in s2
    
    fillRemaining :: (Bits a,Ord a) => Int -> a -> Set a -> Set a
    fillRemaining pos value cur
      | pos == -1 = Set.insert value cur
      | otherwise = let s1 = fillRemaining (pos-1) (setBit value pos) cur
                        s2 = fillRemaining (pos-1) value s1
                    in s2

encodeSingleton :: (Bits a,Monad m) => Int -> a -> BDDM s Int m (Tree s Int)
encodeSingleton off v = encodeSingleton' ((bitSize v) - 1)
  where
    encodeSingleton' n = if n == -1
                            then true
                            else (do
                                     f <- false
                                     res <- encodeSingleton' (n-1)
                                     if testBit v n
                                       then node ((bitSize v)-1-n+off) res f
                                       else node ((bitSize v)-1-n+off) f res)