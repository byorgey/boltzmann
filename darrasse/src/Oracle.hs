module Oracle where

import Data.Array.IArray

data CombEq = N | E | A | S CombEq CombEq | P CombEq CombEq | R Int deriving Show

eval_eq :: Double -> Array Int Double -> CombEq -> Double
eval_eq z y N = 0.0
eval_eq z y E = 1.0
eval_eq z y A = z
eval_eq z y (S e1 e2) = eval_eq z y e1 + eval_eq z y e2
eval_eq z y (P e1 e2) = eval_eq z y e1 * eval_eq z y e2
eval_eq z y (R i) = y ! i

diff_eq :: Int -> CombEq -> CombEq
diff_eq i N = N
diff_eq i E = N
diff_eq i A = N
diff_eq i (S e1 e2) = S (diff_eq i e1) (diff_eq i e2)
diff_eq i (P e1 e2) = S (P (diff_eq i e1) e2) (P e1 (diff_eq i e2))
diff_eq i (R j) | i == j = E
                | otherwise = N

simplify :: CombEq -> CombEq
simplify (S e1 e2) = case ((simplify e1), (simplify e2)) of
                       (N, e) -> e
                       (e, N) -> e
                       (e1',e2') -> S e1' e2'
simplify (P e1 e2) = case ((simplify e1), (simplify e2)) of
                       (N, e) -> N
                       (e, N) -> N
                       (E, e) -> e
                       (e, E) -> e
                       (e1', e2') -> P e1' e2'
simplify e = e

class CombSysState state where
  zeroState :: CombSys -> state
  stateToList :: state -> [Double]
  iter :: CombSys -> Double -> state -> state

type CombSys = Array Int CombEq

has_diverged :: (CombSysState state) => state -> Double -> Bool
has_diverged st prec = or $ map (\x -> isNaN x || x < 0 || x > 1.0/prec) $ stateToList st

eval_sys :: (CombSysState state) => CombSys -> Double -> Double -> state
eval_sys sys z prec =
  aux $ zeroState sys
  where aux y = let y' = iter sys z y in
                if (foldl max 0.0 $ map abs $ zipWith (-) (stateToList y') (stateToList y)) < prec then
                  y'
                else
                  aux y'

sing_sys :: (CombSysState state) => CombSys -> Double -> Double -> (Double, state)
sing_sys sys prec1 prec2 =
  aux 0.0 1.0 `asTypeOf` foo
  where aux zmin zmax | zmax - zmin < prec1 = (zmin, eval_sys sys zmin prec2)
                      | otherwise = if has_diverged (eval_sys sys z prec2 `asTypeOf` snd foo) prec1 then
                                      aux zmin z
                                    else
                                      aux z zmax
                      where z = (zmax + zmin) / 2
        foo = (undefined, undefined) :: (CombSysState state) => (Double, state)
