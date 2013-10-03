module Oracle.Simple where

import Oracle
import Data.Array.IArray

newtype State = SimpleState (Array Int Double)
instance CombSysState State where
  zeroState sys = SimpleState (listArray (bounds sys) (repeat 0.0))
  stateToList (SimpleState s) = elems s
  iter sys z (SimpleState s) = SimpleState $ amap (eval_eq z s) sys
