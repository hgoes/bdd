module Data.BDD.Integer where

import Data.BDD.Internals
import Data.Bits

encodeSignedRange :: (Ord a,Monad m,Bits a) => Int -> a -> a -> BDDM s Int m (Tree s Int)
encodeSignedRange off l u
    | l >= 0 = encodeRange off l u
    | u < 0  = encodeRange off l u
    | otherwise = do
        ln <- encodeRange off l (-1)
        rn <- encodeRange off 0 u
        ln #|| rn

encodeRange :: (Ord a,Monad m,Bits a) => Int -> a -> a -> BDDM s Int m (Tree s Int)
encodeRange off l u
    | l <= u = encodeRange' l u ((bitSize l) - 1)
    | otherwise = error "Lower bound is greater than upper bound"
      where
        encodeRange' :: (Monad m,Bits a) => a -> a -> Int -> BDDM s Int m (Tree s Int)
        encodeRange' l u pos
          | pos == -1 = true
          | (testBit l pos) && (testBit u pos) = do
            ln <- encodeRange' l u (pos-1)
            rn <- false
            node n ln rn
          | (not $ testBit l pos) && (not $ testBit u pos) = do
            ln <- false
            rn <- encodeRange' l u (pos-1)
            node n ln rn
          | otherwise = do
            ln <- encodeRangeU' u (pos-1)
            rn <- encodeRangeL' l (pos-1)
            node n ln rn
            where
              n = bitSize l - pos + off - 1

        encodeRangeL' :: (Monad m,Bits a) => a -> Int -> BDDM s Int m (Tree s Int)
        encodeRangeL' l pos
          | pos == -1 = true
          | testBit l pos = do
            ln <- encodeRangeL' l (pos-1)
            rn <- false
            node n ln rn
          | otherwise = do
            ln <- true
            rn <- encodeRangeL' l (pos-1)
            node n ln rn
            where
              n = bitSize l - pos + off - 1

        encodeRangeU' :: (Monad m,Bits a) => a -> Int -> BDDM s Int m (Tree s Int)
        encodeRangeU' u pos
          | pos == -1 = true
          | testBit u pos = do
            ln <- encodeRangeU' u (pos-1)
            rn <- true
            node n ln rn
          | otherwise = do
            ln <- false
            rn <- encodeRangeU' u (pos-1)
            node n ln rn
            where
              n = bitSize u - pos + off - 1
{-

0123 4567

0000 0000
1111 1110

-}