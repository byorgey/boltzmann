module Gen (toGen) where

import           Combstruct
import           Control.Monad
import           Data.Generics
import           Data.List
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Def
import           Test.QuickCheck

-- TODO remove Int from Var

-- | Construct a generic definition corresponding to the type of the
--   argument (the argument itself is ignored).
toDef :: Data a => a -> Def
toDef x = Def (dataTypeOf x) s
  where
    s = Sum $ map mkProd argDataTypes

    -- For each constructor, find the data types it contains.
    argDataTypes :: [[DataType]]
    argDataTypes = map (gmapQ dataTypeOf) (constrInstances x)

    -- Take a list of data types (representing arguments to a
    -- constructor) and turn it into a Prod, wrapping each DataType in
    -- a Var (with a dummy index for now).
    mkProd :: [DataType] -> Prod
    mkProd = Prod . map (Var 0 . dataTypeName)


countVars :: Map String Def -> Defs
countVars m = Defs $ Map.map aux m'
  where m' = snd $ Map.mapAccum (\n -> \d -> (n+1, (d, n))) 1 m
        aux (Def d s, i) = (Def d (aux' s), i)
        aux' (Sum ps) = Sum $ map aux'' ps
        aux'' (Prod rs) = Prod $ map aux''' rs
        aux''' (Var 0 n) = case Map.lookup n m' of
                             Just (_, i) -> Var i n
                             Nothing     -> error "Impossible: unknown reference"
        aux''' (Var i n) = Var i n

toDefs :: Data a => a -> Defs
toDefs x = countVars $ aux x Map.empty
  where aux :: Data a => a -> Map String Def -> Map String Def
        aux y m = case Map.lookup n m of
                    Just r  -> m
                    Nothing -> foldl sub (Map.insert n (toDef y) m) (constrInstances y)
                    where n = dataTypeName $ dataTypeOf y
        sub m   = gmapQr ($) m aux

-- | Get one instance for each constructor of the argument's type.
--   For example,  @constrInstances (Just 3) = [Nothing, Just undefined]@.
constrInstances :: Data a => a -> [a]
constrInstances = map fromConstr . dataTypeConstrs . dataTypeOf

toOracle :: Data a => a -> Oracle
toOracle x = defsToOracle $ toDefs x

toGenAux :: Data a => Oracle -> a -> Gen a
toGenAux o x = do gmapM (toGenAux o) . floatToInstance =<< choose (0, sum po)
  where Def d (Sum l) = toDef x
        floatToInstance = fromConstr . indexConstr d . findConstrIndex 1 po
        findConstrIndex n (x:xs) r = if r < x then n
                                     else findConstrIndex (n+1) xs (r-x)
        po = map prodOracle l
        prodOracle (Prod l) = product $ map refOracle l
        refOracle  (Var i s)  = Map.findWithDefault 0 s o

toGen :: Data a => a -> Gen a
-- toGen x | trace (show $ toOracle x) False = undefined
toGen x = toGenAux (toOracle x) x
