module Oracle.Simple where

import           Data.Array.IArray
import           Oracle

newtype State = SimpleState (Array Int Double)
instance CombSysState State where
  zeroState sys = SimpleState (listArray (bounds sys) (repeat 0.0))
  stateToList (SimpleState s) = elems s
  iter sys z (SimpleState s) = SimpleState $ amap (eval z s) sys
