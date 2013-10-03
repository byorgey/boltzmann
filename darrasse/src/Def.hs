module Def where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map

data Sum  = Sum [Prod]
data Prod = Prod [Ref]
data Ref  = Ref Int String

data Def = Def DataType Sum

newtype Defs = Defs (Map String (Def, Int))
