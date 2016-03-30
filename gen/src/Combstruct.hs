module Combstruct (Oracle, defsToOracle) where

import           Data.Array.IArray
import           Data.Generics     (dataTypeName)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Def
import           Oracle
-- import Oracle.Simple
import           Oracle.Newton

type Oracle = Map String Double

varToComb :: Var -> CombExp
varToComb (Var 0 _) = error "Impossible: nil reference"
varToComb (Var i _) = Y i

prodToComb :: Prod -> CombExp
prodToComb (Prod rs) = foldl (:*:) X $ map varToComb rs

sumToComb :: Sum -> CombExp
sumToComb (Sum ps) = foldl1 (:+:) $ map prodToComb ps

defToComb :: Def -> CombExp
defToComb (Def _ s) = sumToComb s

defsToComb :: Defs -> CombSys
defsToComb (Defs m) =
  array (1,Map.size m) $ map (\(d,i) -> (i,defToComb d)) (Map.elems m)

singPrecision :: Double
singPrecision = 1.0e-10

valPrecision :: Double
valPrecision = 1.0e-6

combToVals :: CombSys -> [Double]
combToVals
  = stateToList
  . (id :: State -> State)
  . snd
  . singularity singPrecision valPrecision

defsToOracle :: Defs -> Oracle
defsToOracle (Defs m) =
  Map.fromList $ zip (map (\((Def d _), _) -> dataTypeName d) (Map.elems m)) $ vals
  where vals = combToVals $ defsToComb (Defs m)
