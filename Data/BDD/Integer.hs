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
    | l <= u = encodeRange' off l u (bitSize l) 0
    | otherwise = error "Lower bound is greater than upper bound"
  
encodeRange' :: (Monad m,Bits a) => Int -> a -> a -> Int -> Int -> BDDM s Int m (Tree s Int)
encodeRange' off l u fpos pos
    | pos == fpos = true
    | (testBit l n) && (testBit u n) = do
        ln <- false
        rn <- encodeRange' off l u fpos (pos + 1)
        node pos ln rn
    | (not $ testBit l n) && (not $ testBit u n) = do
        ln <- encodeRange' off l u fpos (pos + 1)
        rn <- false
        node pos ln rn
    | otherwise = do
        ln <- encodeRangeL' off l fpos (pos + 1)
        rn <- encodeRangeU' off u fpos (pos + 1)
        node pos ln rn
    where
      n = fpos - pos + off - 1

encodeRangeL' :: (Monad m,Bits a) => Int -> a -> Int -> Int -> BDDM s Int m (Tree s Int)
encodeRangeL' off l fpos pos
    | pos == fpos = true
    | testBit l n = do
        ln <- false
        rn <- encodeRangeL' off l fpos (pos + 1)
        node pos ln rn
    | otherwise = do
        ln <- encodeRangeL' off l fpos (pos + 1)
        rn <- true
        node pos ln rn
    where
      n = fpos - pos + off - 1

encodeRangeU' :: (Monad m,Bits a) => Int -> a -> Int -> Int -> BDDM s Int m (Tree s Int)
encodeRangeU' off u fpos pos
    | pos == fpos = true
    | not $ testBit u n = do
        ln <- encodeRangeU' off u fpos (pos + 1)
        rn <- false
        node pos ln rn
    | otherwise = do
        ln <- true
        rn <- encodeRangeU' off u fpos (pos + 1)
        node pos ln rn
    where
      n = fpos - pos + off - 1
{-

0123 4567

0000 0000
1111 1110

-}