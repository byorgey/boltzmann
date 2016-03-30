module Def where

import           Data.Generics
import           Data.Map      (Map)
import qualified Data.Map      as Map

-- | A sum of products.
data Sum  = Sum [Prod]

-- | A product is a list of variables.
data Prod = Prod [Var]

-- | A variable is identified by an Int index.  It also stores the
--   name of the data type originally corresponding to the variable.
data Var  = Var Int String

-- | A generic definition of a particular data type as a
--   sum-of-products.
data Def = Def DataType Sum

-- | A group of definitions is a mapping from names to definitions
--   paired with their corresponding variable index.  (???)
newtype Defs = Defs (Map String (Def, Int))
