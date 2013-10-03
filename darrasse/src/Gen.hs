module Gen (toGen) where
import Data.Generics
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Test.QuickCheck
import Monad
import Def
import Combstruct
-- import Debug.Trace

-- TODO remove Int from Ref

toDef x = Def (dataTypeOf x) sum
  where sum  = Sum $ map prod argDataTypes
        prod = Prod . map (Ref 0 . dataTypeName)
        argDataTypes = map (gmapQ dataTypeOf) (constrInstances x)

countRefs :: Map String Def -> Defs
countRefs m = Defs $ Map.map aux m'
  where m' = snd $ Map.mapAccum (\n -> \d -> (n+1, (d, n))) 1 m
        aux (Def d s, i) = (Def d (aux' s), i)
        aux' (Sum ps) = Sum $ map aux'' ps
        aux'' (Prod rs) = Prod $ map aux''' rs
        aux''' (Ref 0 n) = case Map.lookup n m' of
                             Just (_, i) -> Ref i n
                             Nothing     -> error "Impossible: unknown reference"
        aux''' (Ref i n) = Ref i n

toDefs x = countRefs $ aux x Map.empty
  where aux :: (Data a) => a -> Map String Def -> Map String Def
        aux y m = case Map.lookup n m of
                    Just r  -> m
                    Nothing -> foldl sub (Map.insert n (toDef y) m) (constrInstances y)
                    where n = dataTypeName $ dataTypeOf y
        sub m   = gmapQr ($) m aux

-- get one instance for each constructor of the argument's type
constrInstances :: (Data a) => a -> [a]
constrInstances = map fromConstr . dataTypeConstrs . dataTypeOf

toOracle :: (Data a) => a -> Oracle
toOracle x = defsToOracle $ toDefs x

toGenAux :: (Data a) => Oracle -> a -> Gen a
toGenAux o x = do gmapM (toGenAux o) . floatToInstance =<< choose (0, sum po)
  where Def d (Sum l) = toDef x
        floatToInstance = fromConstr . indexConstr d . findConstrIndex 1 po
        findConstrIndex n (x:xs) r = if r < x then n
                                     else findConstrIndex (n+1) xs (r-x)
        po = map prodOracle l
        prodOracle (Prod l) = product $ map refOracle l
        refOracle  (Ref i s)  = Map.findWithDefault 0 s o

toGen :: (Data a) => a -> Gen a
-- toGen x | trace (show $ toOracle x) False = undefined
toGen x = toGenAux (toOracle x) x
