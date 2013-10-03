{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

import           System.Environment             (getArgs)

import           Control.Applicative
import           Control.Lens                   hiding (( # ))
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

data Tree = Leaf | Branch Tree Tree
  deriving Show

size :: Tree -> Int
size Leaf = 1
size (Branch l r) = 1 + size l + size r

toBTree Leaf = Empty
toBTree (Branch l r) = BNode () (toBTree l) (toBTree r)

-----------------------------------------------------------------
-- Generate trees using a critical size-limited Boltzmann sampler

genTreeCrit :: ReaderT Int (StateT Int (RandT StdGen Maybe)) Tree
genTreeCrit = do
  r <- getRandom
  if r <= (1/2 :: Double)
    then atom >> return Leaf
    else atom >> (Branch <$> genTreeCrit <*> genTreeCrit)

atom :: ReaderT Int (StateT Int (RandT StdGen Maybe)) ()
atom = do
  targetSize <- ask
  curSize <- get
  when (curSize >= targetSize) mzero
  put (curSize + 1)

genOneTree :: Int -> Double -> IO (Maybe Tree)
genOneTree size eps = do
  g <- newStdGen
  let sizeWiggle = floor $ fromIntegral size * eps
      maxSz = size + sizeWiggle
      minSz = size - sizeWiggle
  let mt = (evalRandT ?? g) . (runStateT ?? 0) . (runReaderT ?? maxSz) $ genTreeCrit
  case mt of
    Nothing -> return Nothing
    Just (t,sz) -> if sz >= minSz then return (Just t) else return Nothing

genTree' :: Int -> Double -> StateT Int IO Tree
genTree' size eps = do
  mt <- liftIO (genOneTree size eps)
  modify (+1)
  case mt of
    Nothing -> genTree' size eps
    Just t  -> return t

genTree :: Int -> Double -> IO Tree
genTree size eps = evalStateT (genTree' size eps) 0

------------------------------------------------------------
-- Generate trees via pointing.

{-

T(x)  = (1 - sqrt(1 - 4x^2))/(2x)
T*(x) = T(x)/(1 - 2xT(x))

for C(x), E_x(N) = x C'(x)/C(x).  So we set x T*'(x)/T*(x) = 1000 and
solve numerically, finding x =~ 0.49974216368421037.

T = X + XT^2
T* = X* + X*T^2 + XT*T + XTT*

To generate T, we pick X with probability x/(x + x*T(x)^2) = 1/(1 + T(x)^2) = 0.516055... and so on.

For T*, we pick X* with probability x/(x + xT(x)^2 + 2xT(x)T*(x)), and so on.

-}

t :: Double -> Double
t x = (1 - sqrt(1 - 4*x^2)) / (2*x)

tStar :: Double -> Double
tStar x = t x / (1 - 2*x*t(x))

-- found this value using Sage to numerically solve the equation as
-- given above (using find_root)
x1000 = 0.49974216368421037

bern :: MonadRandom m => [(Double, m a)] -> m a
bern choices = do
  x <- getRandomR (0,1)
  let vals  = map fst choices
      probs = map (/sum vals) $ scanl1 (+) vals
      choices' = zip probs (map snd choices)
      choice = head $ dropWhile ((< x) . fst) choices'
  snd choice

infix 1 ==>
(==>) = (,)

genT :: (MonadRandom m, Applicative m) => Double -> m Tree
genT x = bern [ x           ==> return Leaf
              , x * (t x)^2 ==> Branch <$> genT x <*> genT x
              ]

genTStar :: (MonadRandom m, Applicative m) => Double -> m Tree
genTStar x
  = bern
    [ x ==> return Leaf
    , x * (t x)^2 ==> Branch <$> genT x <*> genT x
    , x * (tStar x) * t x ==> Branch <$> genTStar x <*> genT x
    , x * (tStar x) * t x ==> Branch <$> genT x <*> genTStar x
    ]

genTreeP1000 :: (MonadRandom m, Applicative m) => m (Tree, Int)
genTreeP1000 = do
  t <- genTStar x1000
  let s = size t
  if s >= 900 && s <= 1100
    then return (t,s)
    else genTreeP1000

-- it seems to work! =D

n = 1000

{-
main = do
  ts <- replicateM n genTreeP1000
  print . (/fromIntegral n) . fromIntegral . sum . map snd $ ts

-- archimedes :: research/species/boltzmann » time ./GenTree
-- 997.91
-- ./GenTree  52.36s user 0.25s system 99% cpu 52.825 total

-}

main = do
  [sz] <- getArgs
  ts <- replicateM n (genTree (read sz) 0.1)
  print . (/fromIntegral n) . fromIntegral . sum . map size $ ts

-- archimedes :: research/species/boltzmann » time ./GenTree
-- 993.542
-- ./GenTree  32.92s user 0.20s system 99% cpu 33.253 total

{-

archimedes :: research/species/boltzmann » time ./GenTree 50                                   [ 9:58PM]
49.682
./GenTree 50  1.37s user 0.01s system 99% cpu 1.387 total
archimedes :: research/species/boltzmann » time ./GenTree 100                                  [ 9:58PM]
99.474
./GenTree 100  3.11s user 0.02s system 99% cpu 3.152 total
archimedes :: research/species/boltzmann » time ./GenTree 200                                  [ 9:58PM]
198.494
./GenTree 200  6.82s user 0.04s system 99% cpu 6.876 total
archimedes :: research/species/boltzmann » time ./GenTree 400                                  [ 9:58PM]
398.798
./GenTree 400  13.08s user 0.08s system 99% cpu 13.208 total
archimedes :: research/species/boltzmann » time ./GenTree 800                                  [ 9:58PM]
795.798
./GenTree 800  25.99s user 0.16s system 99% cpu 26.228 total

-}

{-
drawT = maybe mempty (renderTree (const (circle 0.05 # fc black)) (~~))
      . symmLayoutBin' with { slVSep = 0.5 } . toBTree

main = do
  t <- genTStar x1000
  defaultMain (drawT t # centerXY # pad 1.1 # sized (Width 4))
-}


----------------------------------------------------------------------
-- Size-limited critical Boltzmann sampler again; this time using
-- laziness, as suggested by Jonas Duregård (XXX link)

genTreeLazy :: Rand StdGen Tree
genTreeLazy = do
  r <- getRandom
  if (r < (1/2 :: Double))
    then return Leaf
    else runSplit Branch genTreeLazy genTreeLazy

runSplit :: RandomGen g => (a -> b -> c) -> Rand g a -> Rand g b -> Rand g c
runSplit f m1 m2 = do
  g' <- getSplit
  a <- m1
  let b = evalRand m2 g'
  return $ f a b

data Nat = Z | S Nat

peanoSize :: Tree -> Nat
peanoSize t = peanoSize' t Z
  where
    peanoSize' :: Tree -> (Nat -> Nat)
    peanoSize' Leaf = id
    peanoSize' (Branch l r) = S . peanoSize' l . peanoSize' r

cmpNat :: Nat -> Nat -> Ordering
cmpNat Z Z         = EQ
cmpNat Z (S _)     = LT
cmpNat (S _) Z     = GT
cmpNat (S m) (S n) = cmpNat m n
