{-# LANGUAGE DeriveDataTypeable #-}

import           Gen
import           GHC.Generics
import           Test.QuickCheck.Gen

import           Data.Data
import           Data.Typeable

data Prog = Prog Dec Stat deriving (Typeable, Data, Show)
data Dec  = Nodec | Ondec Id Type | Manydecs Dec Dec deriving (Typeable, Data, Show)
data Id   = A | B deriving (Typeable, Data, Show)
data Type = Int | Bool deriving (Typeable, Data, Show)
data Stat = Noop | Assign Id Exp | Seq Stat Stat deriving (Typeable, Data, Show)
data Exp  = Zero | Succ Exp deriving (Typeable, Data, Show)

main = do sample $ toGen $ (undefined::Prog)
