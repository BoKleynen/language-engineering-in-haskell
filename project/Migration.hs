{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Migration where

import Control.Monad.State (State, MonadState (state))


example :: MigrationM ()
example = createTable "movies" do
  integer "id" PK NotNull
  string "title" NotNull
  string "director" NotNull
  unique ["director", "title"]

newtype MigrationM a = MigrationM { unMigrationM :: IO a }
  deriving (Functor, Applicative, Monad)

newtype TableM a = TableM { unTableM :: State TableDefinition a }
  deriving (Functor, Applicative, Monad)

data TableDefinition = TableDefinition
  { tableName :: String
  , columns :: [ColumnDefinition]
  , tableconstraints :: [TableConstraint]
  }

createTable :: String -> TableM a -> MigrationM ()
createTable _name _table = error  "TODO"

data ColumnDefinition = ColumnDefinition
  { columnName :: String
  , typ :: ColumnType
  , constraints :: [Constraint]
  }

data ColumnType
  = String
  | Int
  | BigInt
  | Bool
  | Raw String

data Constraint
  = PK
  | NotNull
  | Unique
  | Check String
  | Default String

createColumn :: CreateColumnType r => ColumnType -> String -> r
createColumn typ name = createColumn_ typ name []

class CreateColumnType r where
  createColumn_ :: ColumnType -> String -> [Constraint] -> r

instance a ~ ColumnDefinition => CreateColumnType (TableM a) where
  createColumn_ :: ColumnType -> String -> [Constraint]  -> TableM ColumnDefinition
  createColumn_ typ name constraints = TableM $ state addColumn
    where
        addColumn t@TableDefinition{..} =
          let col = ColumnDefinition name typ constraints
          in (col, t { columns = col : columns })

instance CreateColumnType r => CreateColumnType (Constraint -> r) where
  createColumn_ :: ColumnType -> String -> [Constraint] ->  Constraint -> r
  createColumn_ typ name constraints constraint = createColumn_ typ name (constraint : constraints)

string :: CreateColumnType r => String -> r
string = createColumn String

integer :: CreateColumnType r => String -> r
integer = createColumn Int

bigInt :: CreateColumnType r => String -> r
bigInt = createColumn BigInt

bool :: CreateColumnType r => String -> r
bool = createColumn Bool

raw :: CreateColumnType r => String -> String -> r
raw name typ = createColumn (Raw typ) name


data TableConstraint = TableConstraint TableConstraintKind (Maybe String)

data TableConstraintKind
  = UniqueTC [String]
  | CheckTC String

unique :: [String] -> TableM TableConstraint
unique columns = constraint $ TableConstraint (UniqueTC columns) Nothing

check :: String -> TableM TableConstraint
check expr = constraint $ TableConstraint (CheckTC expr) Nothing

constraint :: TableConstraint -> TableM TableConstraint
constraint tc = TableM $ state addConstraint
  where
    addConstraint t@TableDefinition{..} = (tc, t { tableconstraints = tc : tableconstraints })
