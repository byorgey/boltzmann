module Combstruct (Oracle, defsToOracle) where

import Data.Generics (dataTypeName)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Array.IArray
import qualified Data.Map as Map
import Def
import Oracle
-- import Oracle.Simple
import Oracle.Newton

type Oracle = Map String Double

refToComb :: Ref -> CombEq
refToComb (Ref 0 s) = error "Impossible: nil reference"
refToComb (Ref i s) = R i

prodToComb :: Prod -> CombEq
prodToComb (Prod rs) = foldl P A $ map refToComb rs

sumToComb :: Sum -> CombEq
sumToComb (Sum ps) = foldl1 S $ map prodToComb ps

defToComb :: Def -> CombEq
defToComb (Def _ s) = sumToComb s

defsToComb :: Defs -> CombSys
defsToComb (Defs m) =
  array (1,Map.size m) $ map (\(d,i) -> (i,defToComb d)) (Map.elems m)

singPrecision :: Double
singPrecision = 1.0e-10

valPrecision :: Double
valPrecision = 1.0e-6

combToVals :: CombSys -> [Double]
combToVals sys = stateToList st
               where st :: State
                     st = snd $ sing_sys sys singPrecision valPrecision

defsToOracle :: Defs -> Oracle
defsToOracle (Defs m) =
  Map.fromList $ zip (map (\((Def d _), _) -> dataTypeName d) (Map.elems m)) $ vals
  where vals = combToVals $ defsToComb (Defs m)
