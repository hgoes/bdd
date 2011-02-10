module Main where

import Test.QuickCheck
import Data.BDD
import Data.BDD.Sets
import Data.BDD.Integer

import Data.Set as Set
import Data.Int

encodeDecodeId :: Int -> [Int] -> Bool
encodeDecodeId off nums = let nset = Set.fromList nums
                              nset2 = runIdBDDM $ do
                                tree <- encodeSet off nset
                                return $ decodeSet off tree
                          in nset == nset2

encodeDecodeRange :: Int -> Int -> Bool
encodeDecodeRange l h = let set1 = Set.fromAscList [l..h]
                            set2 = runIdBDDM $ do
                              tree <- encodeSignedRange 0 l h
                              return $ decodeSet 0 tree
                        in set1 == set2

main = do
  quickCheck $ forAll (suchThat arbitrary (\(x,y) -> x < y)) (\(x,y) -> y > 100000 || x < -100000 || y-x > 10000 || encodeDecodeRange x y)
  quickCheck encodeDecodeId
  