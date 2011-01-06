-- | Functions for reading and writing BDDs to disk
module Data.BDD.Serialization where

import Data.BDD.Internals
import Data.Map as Map
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString.Lazy as LBS (writeFile,readFile)
import System.Directory (doesFileExist)
import Data.Unique
import Control.Monad.Reader

-- | Use the binary library to serialize a BDD.
serializeTree :: Binary t => Tree s t -> Put
serializeTree tree = let (loc,act,_,_) = serialize Map.empty 0 tree
                     in put loc >> act
  where
    serialize :: Binary t => Map Unique Int -> Int -> Tree s t -> (Int,Put,Map Unique Int,Int)
    serialize mp pos (Leaf _ v) = (if v then -1 else -2,
                                   return (),
                                   mp,
                                   pos)
    serialize mp pos (Node un val l r) = case Map.lookup un mp of
      Just loc -> (loc,return (),mp,pos)
      Nothing -> let (lloc,lact,nmp1,npos1) = serialize (Map.insert un pos mp) (pos+1) l
                     (rloc,ract,nmp2,npos2) = serialize nmp1 npos1 r
                in (0,
                    put val >> put lloc >> put rloc >> lact >> ract,
                    nmp2,
                    npos2)

deserializeTree :: (Binary t,Ord t) => BDDM s t Get (Tree s t)
deserializeTree = do
  t <- true
  f <- false
  loc <- lift get
  (tree,_,_) <- deserialize t f Map.empty 0 loc
  return tree
  where
    deserialize :: (Binary t,Ord t) => Tree s t -> Tree s t -> Map Int (Tree s t) -> Int -> Int -> BDDM s t Get (Tree s t,Map Int (Tree s t),Int)
    deserialize t f mp pos loc = case loc of
        -1 -> return (t,mp,pos)
        -2 -> return (f,mp,pos)
        0 -> do
          val <- lift get
          lloc <- lift get
          rloc <- lift get
          (ltree,nmp1,npos1) <- deserialize t f mp (pos+1) lloc
          (rtree,nmp2,npos2) <- deserialize t f nmp1 npos1 rloc
          ntree <- node val ltree rtree
          return (ntree,Map.insert pos ntree nmp2,npos2)
        _ -> case Map.lookup loc mp of
          Nothing -> error $ "Internal deserialization error at "++show loc++show (Map.keys mp)
          Just tree -> return (tree,mp,pos)

writeBDD :: (Binary t,MonadIO m) => FilePath -> Tree s t -> BDDM s t m ()
writeBDD fp tree = liftIO $ LBS.writeFile fp (runPut $ serializeTree tree)

readBDD :: (Binary t,Ord t,MonadIO m) => FilePath -> BDDM s t m (Tree s t)
readBDD fp = do
  str <- liftIO $ LBS.readFile fp
  act <- debase deserializeTree
  return $ runGet act str

-- | Cache a BDD in a file. If the file exists, the BDD is loaded from it. If it doesn't, the tree is built
--   and stored into the file.
cachedBDD :: (MonadIO m,Binary t,Ord t) => FilePath -- ^ The filename of the cache
          -> BDDM s t m (Tree s t)                 -- ^ The action to rebuild the cache
          -> BDDM s t m (Tree s t)
cachedBDD fp act = do
  exists <- liftIO $ doesFileExist fp
  if exists
    then readBDD fp
    else (do
           tree <- act
           writeBDD fp tree
           return tree)
