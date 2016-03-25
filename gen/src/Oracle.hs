{-# LANGUAGE ScopedTypeVariables #-}

module Oracle where

import           Data.Array.IArray
import           Data.Function     (on)
import           Data.List         (foldl')

-- | Combinatorial expressions in a variable X, which can make up a
--   combinatorial system of the form (Y_0, Y_1, ..., Y_n) = (exp_0,
--   exp_1, ..., exp_n).  They consist of 0, 1, X, sums, products, and
--   references to other Y_i.
data CombExp
  = Zero
  | One
  | X
  | (:+:) CombExp CombExp
  | (:*:) CombExp CombExp
  | Y Int
  deriving Show

-- | A combinatorial system is an array of combinatorial expressions.
type CombSys = Array Int CombExp

-- | Evaluate a combinatorial expression, given a value for the
--   variable X and values for all the y_i.
eval :: Double -> Array Int Double -> CombExp -> Double
eval z y Zero        = 0
eval z y One         = 1
eval z y X           = z
eval z y (e1 :+: e2) = eval z y e1 + eval z y e2
eval z y (e1 :*: e2) = eval z y e1 * eval z y e2
eval z y (Y i)       = y ! i

-- | Partial differentiation with respect to one of the y_i.
diff :: Int -> CombExp -> CombExp
diff i Zero        = Zero
diff i One         = Zero
diff i X           = Zero  -- Note this is zero, not one, since we are
                           -- differentiating with respect to y_i, not
                           -- X.
diff i (e1 :+: e2) = (diff i e1) :+: (diff i e2)
diff i (e1 :*: e2) = (diff i e1 :*: e2) :+: (e1 :*: diff i e2)
diff i (Y j) | i == j    = One
             | otherwise = Zero

-- | Simplify a combinatorial expression, recursively removing trivial
--   sums and products.  Note this doesn't distribute products over
--   sums or anything more sophisticated.
simplify :: CombExp -> CombExp
simplify (e1 :+: e2) =
  case (simplify e1, simplify e2) of
    (Zero, e)  -> e
    (e, Zero)  -> e
    (e1', e2') -> e1' :+: e2'
simplify (e1 :*: e2) =
  case (simplify e1, simplify e2) of
    (Zero, e)  -> Zero
    (e, Zero)  -> Zero
    (One, e)   -> e
    (e, One)   -> e
    (e1', e2') -> e1' :*: e2'
simplify e = e

-- | Types which keep track of the state needed for a combinatorial
--   system solver.
class CombSysState s where
  zeroState   :: CombSys -> s                 -- ^ Initial state
  stateToList :: s -> [Double]                -- ^ Get a current list of values for Y_i
  iter        :: CombSys -> Double -> s -> s  -- ^ Perform one iteration

-- | Check whether a solver has diverged: when any of the XXX values
--   are NaN, negative, or too big (defined as 1 divided by the
--   precision).
hasDiverged :: (CombSysState s) => Double -> s -> Bool
hasDiverged prec = any (\x -> isNaN x || x < 0 || x > 1.0/prec) . stateToList

-- | Check whether a solver has converged, i.e. the values of two
--   consecutive states have not changed by more than the specified
--   precision.
hasConverged :: (CombSysState s) => Double -> s -> s -> Bool
hasConverged prec s1 s2 = (foldl' max 0 $ (zipWith absDiff `on` stateToList) s1 s2) < prec
  where
    absDiff a b = abs (a - b)

-- | Evaluate a combinatorial system at a given value of z.
--   Evaluation proceeds by iterating until the values change by less
--   than the specified precision.
evalSys :: (CombSysState s) => CombSys -> Double -> Double -> s
evalSys sys z prec = go $ zeroState sys
  where
    go y
      | hasConverged prec y y' =    y'
      | otherwise              = go y'
      where
        y' = iter sys z y

-- | Find the least singularity of a combinatorial system on the
--   interval [0,1] using binary search.  That is, we want to find the
--   smallest value of z on [0,1] such that the system diverges when
--   evaluated at z.  Returns both the value of z as well as the state
--   of the system at that z.
--
--   The first argument @precS@ is the search precision: the search
--   will be stopped when the size of the search interval is smaller
--   than @precS@; also, the combinatorial system will be considered
--   to have diverged when the value of one of the y_i is larger than
--   @1/precS@.  The second argument @precV@ is the value precision:
--   when evaluating the combinatorial system, we continue iterating
--   until the values change by less than @precV@.
singularity :: forall s. CombSysState s => Double -> Double -> CombSys -> (Double, s)
singularity precS precV sys = go 0 1
  where
    go :: CombSysState s => Double -> Double -> (Double, s)
    go zmin zmax
      -- If we've converged sufficiently, just return zmin
      | zmax - zmin < precS                             = (zmin, evalSys sys zmin precV)
      -- If the system diverges @ zmid, then zmid is too big
      | hasDiverged precS (evalSys sys zmid precV :: s) = go zmin zmid
      -- Otherwise, zmid is too small
      | otherwise                                       = go zmid zmax
      where zmid = (zmax + zmin) / 2
