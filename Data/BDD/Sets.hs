{-# LANGUAGE ScopedTypeVariables #-}
module Data.BDD.Sets
    (encodeSet,
     decodeSet,
     encodeSingleton
    ) where

import Data.BDD.Internals
import Data.Set as Set
import Data.Bits

encodeSet :: (Bounded a,Enum a,Ord a,Monad m) =>
            Int   -- ^ The offset from which to start encoding the set
          -> Set a -- ^ The set to encode 
          -> BDDM s Int m (Tree s Int)
encodeSet off (set::Set a) = encodeSet' off (\v -> toEnum (rmin+v)) 0 fpos 0 rmax set
  where
    fpos = ceiling $ logBase 2 (fromIntegral $ rmax-rmin+1)
    rmin :: Int
    rmin = fromEnum vmin
    rmax :: Int
    rmax = fromEnum vmax
    vmin :: a
    vmin = minBound
    vmax :: a
    vmax = maxBound

encodeSet' :: (Ord a,Monad m) => Int -> (Int -> a) -> Int -> Int -> Int -> Int -> Set a -> BDDM s Int m (Tree s Int)
encodeSet' off f pos fpos value limit set 
  | pos == fpos = if value > limit
                 then false
                 else (if Set.member (f value) set
                       then true
                       else false)
  | otherwise = do
    s1 <- encodeSet' off f (pos+1) fpos (value .|. (1 `shiftL` pos)) limit set
    s2 <- encodeSet' off f (pos+1) fpos value limit set
    node (pos+off) s1 s2

decodeSet :: (Bounded a,Enum a,Ord a) => Int -> Tree s Int -> Set a
decodeSet = decodeSet'' undefined

decodeSet'' :: (Bounded a,Enum a,Ord a) => a -> Int -> Tree s Int -> Set a
decodeSet'' (_::a) off tree = decodeSet' off (\v -> toEnum (rmin+v)) (-1) fpos rmax 0 Set.empty tree
  where
    fpos = ceiling $ logBase 2 (fromIntegral $ rmax-rmin+1)
    rmin :: Int
    rmin = fromEnum vmin
    rmax :: Int
    rmax = fromEnum vmax
    vmin :: a
    vmin = minBound
    vmax :: a
    vmax = maxBound

decodeSet' :: (Ord a) => Int -> (Int -> a) -> Int -> Int -> Int -> Int -> Set a -> Tree s Int -> Set a
decodeSet' off f pos fpos limit value set leaf@(Leaf _ v)
  = if v
    then fillRemaining limit pos fpos value f set
    else set
decodeSet' off f pos fpos limit value set node@(Node _ sym l r)
  | sym - off < 0 = let set' = decodeSet' off f pos fpos limit value set l
                    in decodeSet' off f pos fpos limit value set' r
  | sym - off >= fpos = if value <= limit
                       then fillRemaining limit pos fpos value f set --Set.insert (f value) set
                       else set
  | pos < sym - off = let set' = decodeSet' off f (pos+1) fpos limit (value .|. (1 `shiftL` pos)) set node
                      in decodeSet' off f (pos+1) fpos limit value set' node
  | otherwise = let set' = decodeSet' off f (pos+1) fpos limit (value .|. (1 `shiftL` pos)) set l
                in decodeSet' off f (pos+1) fpos limit value set' r

fillRemaining :: Ord a => Int -> Int -> Int -> Int -> (Int -> a) -> Set a -> Set a
fillRemaining limit pos fpos value f cur
  | pos == fpos = if value <= limit
                 then Set.insert (f value) cur
                 else cur
  | otherwise = let s1 = fillRemaining limit (pos+1) fpos (value .|. (1 `shiftL` pos)) f cur
                    s2 = fillRemaining limit (pos+1) fpos value f s1
                in s2

encodeSingleton :: (Bounded a,Enum a,Ord a,Monad m) => Int -> a -> BDDM s Int m (Tree s Int)
encodeSingleton off (v::a) = encodeSingleton' off 0 fpos (fromEnum v - rmin)
  where
    fpos = ceiling $ logBase 2 (fromIntegral $ rmax-rmin+1)
    rmin :: Int
    rmin = fromEnum vmin
    rmax :: Int
    rmax = fromEnum vmax
    vmin :: a
    vmin = minBound
    vmax :: a
    vmax = maxBound

encodeSingleton' :: Monad m => Int -> Int -> Int -> Int -> BDDM s Int m (Tree s Int)
encodeSingleton' off pos fpos value
  = do
    t <- true
    f <- false
    if pos == fpos
      then return t
      else (do
               res <- encodeSingleton' off (pos+1) fpos value
               if (value .&. (1 `shiftL` pos)) == 0
                 then node (pos+off) f res
                 else node (pos+off) res f
           )
