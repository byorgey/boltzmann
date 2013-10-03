import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random

-- These are surely defined in some Hackage package somewhere, but
-- it's worth implementing them myself to be sure I understand how
-- they work!

-- should figure out the general way to do Geom_>=k etc. -- have to
-- initialize p0 differently.  Use corresponding EGF for
-- e.g. L_>=k(x).
--
-- Can do Geom easily/efficiently -- can use closed form of
-- 1 + x + x^2 + ... + x^k = (x^{k+1} - 1)/(x-1)
--
-- For Pois, need to subtract 1 + x + x^2/2 + x^3/3! + ... + x^k/k!.
-- Don't know of a nice closed form for that but easy enough to
-- compute in O(k) time.
--
-- Again, for Loga, subtract 1 + x + x^2/2 + x^3/3 + ... + x^k/k.
-- Again, easy to compute in O(k) time.

-- | Generate a random integer according to a geometric distribution of
--   parameter p, i.e. P(X=k) = (1-p) p^k   =   L_k(p) / L(p)
geom :: MonadRandom m => Double -> m Int
geom = sequential 0 (\p -> (1-p, const (*p)))

-- This doesn't work yet
-- geom :: MonadRandom m => Int -> Double -> m Int
-- geom k = sequential k (\p -> ((1-p)/(p^k), const (*p)))

-- | Generate a random integer according to a Poisson distribution,
--   i.e. P(X=k) = e^(-p) p^k / k!    =  E_k(p) / E(p)
pois :: MonadRandom m => Double -> m Int
pois = sequential 0 (\p -> (exp (-p), \k pk -> p * pk / (fi k + 1)))

-- | Generate a random integer according to a Poisson distribution,
--   i.e. P(X=k) = 1 / log(1/(1-p)) * p^k / k  =  C_k(p) / C(p)
loga :: MonadRandom m => Double -> m Int
loga = sequential 1 (\p -> (1 / (log (1/(1-p))), \k pk -> p * pk * fi k / (fi k + 1)))

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | The /sequential algorithm/ for generating values from certain
--   kinds of discrete distributions.  The first argument @k0@ is the
--   value of k to start with.  If p_k is the probability of k (so
--   sum_{k>=0} p_k = 1) then the first argument @g@ should be such
--   that @g p = (p_k0, f)@ with @f k p_k = p_{k+1}@.
sequential :: MonadRandom m => Int -> (Double -> (Double, Int -> Double -> Double)) -> Double -> m Int
sequential k0 g p = seq' p0 k0 p0 `liftM` getRandom
  where
    (p0, next) = g p
    seq' s k pk u
      | s >= u    = k
      | otherwise = seq' (s + pk') (k+1) pk' u
        where pk' = next k pk
