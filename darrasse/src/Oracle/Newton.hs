module Oracle.Newton where

import Oracle
import Data.Array.IArray

vecAdd :: Array Int Double -> Array Int Double -> Array Int Double
vecAdd x y = array (bounds x) [(i, x!i + y!i) | i <- [li..ui]]
       where (li,ui) = bounds x

vecSub :: Array Int Double -> Array Int Double -> Array Int Double
vecSub x y = array (bounds x) [(i, x!i - y!i) | i <- [li..ui]]
       where (li,ui) = bounds x

matVecMult :: Array (Int,Int) Double -> Array Int Double -> Array Int Double
matVecMult x y     =  array (li,ui)
                         [(i, sum [x!(i,j) * y!j | j <- range (lj,uj)])
                                       | i <- range (li,ui) ]
           where ((li,lj),(ui,uj)) = bounds x

matAdd :: Array (Int,Int) Double -> Array (Int,Int) Double -> Array (Int,Int) Double
matAdd x y = array (bounds x) [((i,j), x!(i,j) + y!(i,j)) | i <- [li..ui], j <- [lj..uj]]
       where ((li,lj),(ui,uj)) = bounds x

matSub :: Array (Int,Int) Double -> Array (Int,Int) Double -> Array (Int,Int) Double
matSub x y = array (bounds x) [((i,j), x!(i,j) - y!(i,j)) | i <- [li..ui], j <- [lj..uj]]
       where ((li,lj),(ui,uj)) = bounds x

matMult         :: Array (Int,Int) Double -> Array (Int,Int) Double -> Array (Int,Int) Double
matMult x y     =  array (bounds x)
                         [((i,j), sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)])
                                       | i <- range (li,ui),
                                         j <- range (lj,uj) ]
        where ((li,lj),(ui,uj))         =  bounds x

identity :: (Int, Int) -> Array (Int, Int) Double
identity (li, ui) = array ((li,li),(ui,ui)) list
            where dirac i j | i == j    = 1.0
                            | otherwise = 0.0
                  list = [ ((i,j), dirac i j) | i <- [li..ui], j <- [li..ui] ]

jacobian     :: CombSys -> Array (Int, Int) CombEq
jacobian sys = array ((li, li), (ui, ui)) list
             where list = [ ((i,j), simplify (diff_eq j (sys ! i))) | i <- [li..ui], j <- [li..ui] ]
                   (li, ui) = bounds sys

newtype State = NewtonState (Array Int Double, Array (Int, Int) Double)
instance CombSysState State where
  zeroState sys = NewtonState ((listArray (bounds sys) (repeat 0.0)), identity (bounds sys))
  stateToList (NewtonState (y,u)) = elems y
  iter sys z (NewtonState (y,u)) =
    let jac = amap (eval_eq z y) (jacobian sys)
        u' = matAdd (matMult u (matSub (matMult jac u) (matSub u (identity (bounds sys))))) u
        hy = amap (eval_eq z y) sys
        y' = vecAdd (matVecMult u' (vecSub hy y)) y in
    NewtonState (y',u')
