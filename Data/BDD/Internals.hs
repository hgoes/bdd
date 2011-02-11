{-# LANGUAGE RankNTypes,ScopedTypeVariables #-}
module Data.BDD.Internals where
   
import System.Mem.Weak
import Data.Unique
import Data.Map as Map hiding (mapMaybe,map)
import Control.Concurrent.MVar
import Control.Monad.Reader
import Control.Monad.Identity
import System.IO.Unsafe
import Data.Maybe
import Data.Bits
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Foldable (foldlM)

data Tree s a
     = Node {-# UNPACK #-} !Unique {-# UNPACK #-} !a !(Tree s a) !(Tree s a)
     | Leaf {-# UNPACK #-} !Unique {-# UNPACK #-} !Bool

nodeHash :: Tree s a -> Int
nodeHash = hashUnique.getUnique

getUnique :: Tree s a -> Unique
getUnique (Node u _ _ _) = u
getUnique (Leaf u _) = u

instance Eq (Tree s a) where
  x == y = getUnique x == getUnique y

instance Ord (Tree s a) where
  compare x y = compare (getUnique x) (getUnique y)

instance Show a => Show (Tree s a) where
  show (Leaf _ val) = show val
  show (Node _ sym l r) = "("++show sym++" "++show l++" "++show r++")"

data BDD s a = BDD
             { nodes :: MVar (Map (a,Unique,Unique) (Weak (Tree s a)))
             , sinkTrue :: !(Tree s a)
             , sinkFalse :: !(Tree s a)
             }

-- | The BDD monad, with 's' being an anonymous state which shall never be mentioned,
--   'b' being the data type of symbols, 'm' the embedded monad and 'a' the result.
data BDDM s b m a = BDDM (ReaderT (BDD s b) m a)

instance (Functor m,Monad m) => Functor (BDDM s b m) where
    fmap f (BDDM act) = BDDM (fmap f act)

instance Monad m => Monad (BDDM s b m) where
  return x = BDDM (return x)
  (BDDM st) >>= f = BDDM (do
                             v <- st
                             let BDDM st2 = f v
                             st2
                             )

instance MonadIO m => MonadIO (BDDM s b m) where
  liftIO act = BDDM (liftIO act)

instance MonadTrans (BDDM s b) where
  lift act = BDDM (lift act)

instance MonadFix m => MonadFix (BDDM s b m) where
  mfix f = BDDM (mfix (do
                          BDDM res <- f
                          return res
                      )
                )

getReader :: BDDM s b m a -> ReaderT (BDD s b) m a
getReader (BDDM st) = st

graphViz :: Show a => Tree s a -> String
graphViz tree = let (_,str,_,_) = graphViz' 0 Map.empty tree
                in "digraph {\n"++str++"}"

graphViz' :: Show a => Int -> Map Unique Int -> Tree s a -> (Int,String,Int,Map Unique Int)
graphViz' c mp (Leaf uniq val) = case Map.lookup uniq mp of
  Just ol -> (ol,"",c,mp)
  Nothing -> (c,"  node"++show c++"[label="++(if val then "1" else "0")++",shape=box];\n",c+1,Map.insert uniq c mp)
graphViz' c mp (Node uniq sym l r) = case Map.lookup uniq mp of
  Just ol -> (ol,"",c,mp)
  Nothing -> let nmp = Map.insert uniq c mp
                 (v1,str1,nc1,mp1) = graphViz' (c+1) nmp l 
                 (v2,str2,nc2,mp2) = graphViz' nc1 mp1 r
              in (c,"  node"++show c++"[label="++show sym++"];\n  node"++show c++" -> node"++show v1++";\n  node"++show c++" -> node"++show v2++"[style=dashed];\n"++str1++str2,nc2,mp2)

runBDDM :: (forall s. BDDM s b m a) -> m a
runBDDM bddm = runReaderT (getReader bddm) (unsafePerformIO newBDD)

runIdBDDM :: (forall s. BDDM s b Identity a) -> a
runIdBDDM = runIdentity . runBDDM

-- | Get the BDD that is always true
true :: Monad m => BDDM s b m (Tree s b)
true = BDDM (do
                bdd <- ask
                return $ sinkTrue bdd)

false :: Monad m => BDDM s b m (Tree s b)
false = BDDM (do
                bdd <- ask
                return $ sinkFalse bdd)

constant :: Monad m => Bool -> BDDM s b m (Tree s b)
constant True = true
constant False = false

-- | Get the BDD that encodes the function that is true iff a variable has a certain value.
unit :: (Monad m,Ord b) => b -- ^ The variable to check
     -> Bool                -- ^ The value the variable must have for the BDD to be true
     -> BDDM s b m (Tree s b)
unit sym val = do
  t <- true
  f <- false
  if val
    then node sym t f
    else node sym f t

asLeaf :: Tree s b -> Maybe Bool
asLeaf (Leaf _ v) = Just v
asLeaf _ = Nothing

newBDD :: IO (BDD s a)
newBDD = do
  mp <- newMVar Map.empty
  uT <- newUnique
  uF <- newUnique
  return $ BDD
    { nodes = mp
    , sinkTrue = Leaf uT True
    , sinkFalse = Leaf uF False
    }

debugBDD :: Show a => BDD s a -> IO ()
debugBDD bdd = do
  mp <- readMVar (nodes bdd)
  trees <- mapM (\wp -> do
                   rp <- deRefWeak wp
                   case rp of
                     Nothing -> return "invalid"
                     Just tree -> return $ show tree) (Map.elems mp)
  print trees
  

-- | Creates a new BDD node
node :: (Ord a,Monad m) => a -- ^ The symbol of the node
     -> Tree s a            -- ^ The tree for the 'False' path
     -> Tree s a            -- ^ The tree for the 'True' path
     -> BDDM s a m (Tree s a)
node sym l r = BDDM (do
                        bdd <- ask
                        if l==r
                           then return l
                           else return $ unsafePerformIO (node' bdd sym l r))

node' :: Ord a => BDD s a -> a -> Tree s a -> Tree s a -> IO (Tree s a)
node' bdd sym l r = let ul = getUnique l
                        ur = getUnique r
                    in modifyMVar (nodes bdd)
                       (\mp -> let ins = do
                                    uniq <- newUnique
                                    let tree = Node uniq sym l r
                                    ptr <- mkWeakPtr tree
                                          (Just $ modifyMVar_ (nodes bdd)
                                           (\mp2 -> case Map.lookup (sym,ul,ur) mp2 of
                                               Nothing -> return mp2
                                               Just wtree -> do
                                                 mtree <- deRefWeak wtree
                                                 case mtree of
                                                   Nothing -> return $ Map.delete (sym,ul,ur) mp2
                                                   Just _ -> return mp2
                                           )
                                          )
                                    return (Map.insert (sym,ul,ur) ptr mp,tree)
                              in case Map.lookup (sym,ul,ur) mp of
                                Nothing -> ins
                                Just wp -> do
                                  --print "found old node"
                                  mt <- deRefWeak wp
                                  case mt of
                                    Nothing -> ins
                                    Just rt -> return (mp,rt)
                       )

symbol :: Tree s a -> Maybe a
symbol (Node _ sym _ _) = Just sym
symbol _ = Nothing

lhs :: Tree s a -> Tree s a
lhs (Node _ _ l _) = l
lhs _ = error "lhs called on leaf"

rhs :: Tree s a -> Tree s a
rhs (Node _ _ _ r) = r
rhs _ = error "rhs called on leaf"

-- | Eliminate a variable from a BDD by fixing its value to a constant.
eliminate :: (Ord a,Monad m) => Tree s a -- ^ The tree which should be manipulated
          -> a                          -- ^ The variable to be eliminated
          -> Bool                       -- ^ The new value of the variable
          -> BDDM s a m (Tree s a)
eliminate tree sym val = case symbol tree of
  Nothing -> return tree
  Just sym' -> if sym == sym'
                then (if val
                      then return $ lhs tree
                      else return $ rhs tree)
                else (do
                         l <- eliminate (lhs tree) sym val
                         r <- eliminate (rhs tree) sym val
                         node sym' l r)

ite :: (Ord a,Monad m) => Tree s a -> Tree s a -> Tree s a -> BDDM s a m (Tree s a)
ite u v1 v2 = do
  t <- true
  f <- false
  if u==t
     then return v1
     else (if u==f
              then return v2
              else (let z = minimum $ mapMaybe (\t -> symbol t) [u,v1,v2]
                    in do
                      f0 <- eliminate u z False
                      f1 <- eliminate v1 z False
                      f2 <- eliminate v2 z False
                      t0 <- eliminate u z True
                      t1 <- eliminate v1 z True
                      t2 <- eliminate v2 z True
                      w0 <- ite f0 f1 f2
                      w1 <- ite t0 t1 t2
                      node z w1 w0
                   )
          )

(#&&) :: (Ord a,Monad m) => Tree s a -> Tree s a -> BDDM s a m (Tree s a)
(#&&) t1 t2 = do
  f <- false
  ite t1 t2 f

and' :: (Ord a,Monad m) => [Tree s a] -> BDDM s a m (Tree s a)
and' xs = do
  t <- true
  foldlM (#&&) t xs

(#||) :: (Ord a,Monad m) => Tree s a -> Tree s a -> BDDM s a m (Tree s a)
(#||) t1 t2 = do
  t <- true
  ite t1 t t2

or' :: (Ord a,Monad m) => [Tree s a] -> BDDM s a m (Tree s a)
or' xs = do
  f <- false
  foldlM (#||) f xs
  
not' :: (Ord a,Monad m) => Tree s a -> BDDM s a m (Tree s a)
not' (Leaf _ False) = true
not' (Leaf _ True) = false
not' (Node _ s l r) = do
  nl <- not' l
  nr <- not' r
  node s nl nr

(#=>) :: (Ord a,Monad m) => Tree s a -> Tree s a -> BDDM s a m (Tree s a)
(#=>) t1 t2 = do
  t <- true
  ite t1 t2 t

(#==) :: (Ord a,Monad m) => Tree s a -> Tree s a -> BDDM s a m (Tree s a)
(#==) t1 t2 = do
  n <- not' t2
  ite t1 t2 n

debase :: (Monad m,Monad m') => BDDM s t m a -> BDDM s t m' (m a)
debase (BDDM act) = BDDM (mapReaderT return act)

decodeSets :: [(Int,Int,Int)] -> Tree s Int -> [[Int]]
decodeSets descr tree
    = let ndescr = sortBy (comparing (\(_,(off,_,_)) -> off)) $ zip [0..] descr
      in map ((map snd) . (sortBy (comparing fst))) $ dec ndescr tree []
    where
      dec :: [(Int,(Int,Int,Int))] -> Tree s Int -> [(Int,Int)] -> [[(Int,Int)]]
      dec [] tree path = [path]
      dec ((i,(off,bits,limit)):rest) tree path = dec' i off bits limit rest 0 tree path 0

      dec' :: Int -> Int -> Int -> Int -> [(Int,(Int,Int,Int))] -> Int -> Tree s Int -> [(Int,Int)] -> Int -> [[(Int,Int)]]
      dec' i off bits limit rest pos node@(Leaf _ v) path value
          | pos == bits = if v
                          then dec rest node ((i,value):path)
                          else []
          | otherwise = let s1 = dec' i off bits limit rest (pos+1) node path (value .|. (1 `shiftL` pos))
                            s2 = dec' i off bits limit rest (pos+1) node path value
                        in s1 ++ s2
      dec' i off bits limit rest pos node@(Node _ sym l r) path value
          | sym - off < 0 = let s1 = dec' i off bits limit rest pos l path value
                                s2 = dec' i off bits limit rest pos r path value
                            in s1 ++ s2
          | sym - off >= bits = if pos == bits
                                then dec rest node (if value <= limit then ((i,value):path) else path)
                                else (let s1 = dec' i off bits limit rest (pos+1) node path (value .|. (1 `shiftL` pos))
                                          s2 = dec' i off bits limit rest (pos+1) node path value
                                      in s1 ++ s2)
          | pos < sym - off = let s1 = dec' i off bits limit rest (pos+1) node path (value .|. (1 `shiftL` pos))
                                  s2 = dec' i off bits limit rest (pos+1) node path value
                              in s1 ++ s2
          | otherwise = let s1 = dec' i off bits limit rest (pos+1) l path (value .|. (1 `shiftL` pos))
                            s2 = dec' i off bits limit rest (pos+1) r path value
                        in s1 ++ s2

foldBDD :: (a -> c -> c -> c) -> (Bool -> c) -> Tree s a -> c
foldBDD f g (Node _ sym l r) = f sym (foldBDD f g l) (foldBDD f g r)
foldBDD f g (Leaf _ v) = g v